import numpy as np
from scipy.optimize import curve_fit
import matplotlib.pyplot as plt
import numpy as np
from libpysal.weights import lat2W
from esda.moran import Moran
import numpy as np
from joblib import Parallel, delayed

overallseed = 4332
localsimulationseed = 52

np.random.seed(overallseed)
# Define the theoretical relationship to fit the data
def alpha_model(mean, alpha):
    """The theoretical model: sd = alpha * sqrt(mean * (1-mean))"""
    # Avoid sqrt of negative numbers for means slightly > 1 or < 0 due to floating point errors
    mean = np.clip(mean, 0, 1)
    return alpha * np.sqrt(mean * (1 - mean))

class UrbanGrowthModel:
    """
    An Agent-Based Model to simulate urban growth and its effect on the alpha parameter.
    """
    def __init__(self, grid_size=100, neighborhood_size=10, contagion_factor=0.5):
        """
        Initializes the model.

        Args:
            grid_size (int): The size of the square city grid.
            neighborhood_size (int): The size of the sub-grids to calculate statistics.
            contagion_factor (float): The strength of clustering.
                                      0 = purely random growth.
                                      Higher values = more contagious, clustered growth.
        """
        self.grid_size = grid_size
        self.neighborhood_size = neighborhood_size
        self.contagion_factor = contagion_factor
        self.grid = np.zeros((grid_size, grid_size), dtype=int)
        
        # Ensure the grid can be evenly divided into neighborhoods
        if grid_size % neighborhood_size != 0:
            raise ValueError("grid_size must be divisible by neighborhood_size")

    def _get_candidate_parcels(self):
        """
        Identifies pervious parcels and assigns them a development probability.
        The probability is influenced by the number of impervious neighbors.
        """
        # Find all pervious parcels (value = 0)
        pervious_parcels = np.argwhere(self.grid == 0)
        
        # Calculate the number of impervious neighbors for each pervious parcel
        # This is a fast way to do a 2D convolution for neighbor counting
        kernel = np.array([[1, 1, 1], [1, 0, 1], [1, 1, 1]])
        neighbor_counts = np.zeros_like(self.grid, dtype=float)
        neighbor_counts[1:-1, 1:-1] = (
            self.grid[ :-2,  :-2] * kernel[0,0] + self.grid[ :-2, 1:-1] * kernel[0,1] + self.grid[ :-2, 2:] * kernel[0,2] +
            self.grid[1:-1,  :-2] * kernel[1,0]                                     + self.grid[1:-1, 2:] * kernel[1,2] +
            self.grid[2:  ,  :-2] * kernel[2,0] + self.grid[2:  , 1:-1] * kernel[2,1] + self.grid[2:  , 2:] * kernel[2,2]
        )
        
        # Extract neighbor counts for only the pervious parcels
        pervious_neighbor_counts = neighbor_counts[pervious_parcels[:, 0], pervious_parcels[:, 1]]
        
        # Calculate development probability: base probability + contagion bonus
        # The '1' is a base probability to ensure even isolated parcels can develop
        probabilities = 1 + self.contagion_factor * pervious_neighbor_counts
        
        return pervious_parcels, probabilities

    def run_simulation(self, n_steps=None,seed=None):
        """
        Runs the urban growth simulation.
        
        Args:
            n_steps (int, optional): The number of parcels to develop. 
                                     Defaults to filling 95% of the grid.
        
        Returns:
            A tuple of (means, sds) recorded at each step of the simulation.
        """
        np.random.seed(seed)
        if n_steps is None:
            n_steps = int(self.grid_size**2 * 0.75) # Develop 95% of the grid

        # Start with a single impervious seed to kickstart contagious growth
        n_seeds = 5
        for _ in range(n_seeds):
            x, y = np.random.randint(1, self.grid_size - 1, 2)
            self.grid[x, y] = 1
        #seed_x, seed_y = np.random.randint(0, self.grid_size, 2)
        #self.grid[seed_x, seed_y] = 1
        
        means = []
        sds = []

        for step in range(n_steps):
            # Get all potential parcels and their development likelihood
            candidates, probabilities = self._get_candidate_parcels()
            
            if len(candidates) == 0:
                break # Stop if no more parcels can be developed
            
            # Normalize probabilities to sum to 1
            prob_dist = probabilities / np.sum(probabilities)
            
            # Choose a parcel to develop based on the weighted probabilities
            chosen_idx = np.random.choice(len(candidates), p=prob_dist)
            chosen_parcel = candidates[chosen_idx]
            
            # Develop the chosen parcel
            self.grid[chosen_parcel[0], chosen_parcel[1]] = 1
            
            # Every few steps, calculate and record statistics
            if step % 10 == 0:
                mean_imperviousness, sd_imperviousness = self._calculate_stats()
                means.append(mean_imperviousness)
                sds.append(sd_imperviousness)
            if step == (n_steps - 1):
                mi = self._calculate_moran()
        
        return np.array(means), np.array(sds), mi

    def _calculate_stats(self):
        """
        Calculates the mean and standard deviation of imperviousness across neighborhoods.
        """
        # Reshape grid into a collection of neighborhoods
        # (num_neighborhoods_y, num_neighborhoods_x, neighborhood_size, neighborhood_size)
        n_size = self.neighborhood_size
        neighborhoods = self.grid.reshape(self.grid_size // n_size, n_size, 
                                           self.grid_size // n_size, n_size)
        neighborhoods = neighborhoods.transpose(0, 2, 1, 3)
        
        # Calculate the mean (fraction) of imperviousness for each neighborhood
        neighborhood_means = np.mean(neighborhoods, axis=(2, 3))
        
        # Calculate the overall mean and the standard deviation across neighborhoods
        overall_mean = np.mean(neighborhood_means)
        std_dev = np.std(neighborhood_means)

        return overall_mean, std_dev
    def _calculate_moran(self):
        """
        Calculate Moran's I.
        """
        w = lat2W(self.grid_size, self.grid_size)
        mi = Moran(self.grid, w).I
        
        return(mi)

    def analyze_alpha(self, means, sds):
        """
        Fits the theoretical model to the simulation data to find the alpha parameter.
        
        Returns:
            The estimated alpha value.
        """
        # Use curve_fit to find the best alpha value
        # We provide a bound to ensure alpha is non-negative
        popt, _ = curve_fit(alpha_model, means, sds, bounds=(0, 1))
        return popt[0]

# Set up two scenarios: one with low clustering and one with high clustering
contagion_low = 0.0  # Random growth
contagion_high = 50.0 # Highly clustered growth

# Scenario 1: Low Clustering
print(f"Running simulation for low contagion (contagion_factor = {contagion_low})...")
model_low = UrbanGrowthModel(contagion_factor=contagion_low)
means_low, sds_low,mi = model_low.run_simulation(seed=localsimulationseed)
alpha_low = model_low.analyze_alpha(means_low, sds_low)
print(f"Estimated Alpha for low contagion: {alpha_low:.4f}")
print(f"Estimated Morans I for high contagion: {mi:.4f}")

# Scenario 2: High Clustering
print(f"\nRunning simulation for high contagion (contagion_factor = {contagion_high})...")
model_high = UrbanGrowthModel(contagion_factor=contagion_high)
means_high, sds_high,mi = model_high.run_simulation(seed=localsimulationseed)
alpha_high = model_high.analyze_alpha(means_high, sds_high)
print(f"Estimated Alpha for high contagion: {alpha_high:.4f}")
print(f"Estimated Morans I for high contagion: {mi:.4f}")

# Visualization
plt.style.use('default')
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 6), sharey=True)

# Plot for low contagion
ax1.scatter(means_low, sds_low, alpha=0.6, label='Simulated Data')
mean_fit = np.linspace(0, 1, 100)
sd_fit = alpha_model(mean_fit, alpha_low)
ax1.plot(mean_fit, sd_fit, color='red', linestyle='--', 
         label=f'Fitted Model (MCH = {alpha_low:.3f})')
ax1.set_title(f'Low Clustering (Contagion = {contagion_low})', fontsize=20)
ax1.set_xlabel('Mean Imperviousness', fontsize=16)
ax1.set_ylabel('Standard Deviation', fontsize=16)
ax1.legend()
ax1.set_xlim(0, 1)
ax1.set_ylim(0, 0.5)
ax1.tick_params(axis='both', labelsize=15) 

# Plot for high contagion
ax2.scatter(means_high, sds_high, alpha=0.6, label='Simulated Data')
mean_fit = np.linspace(0, 1, 100)
sd_fit = alpha_model(mean_fit, alpha_high)
ax2.plot(mean_fit, sd_fit, color='red', linestyle='--', 
         label=f'Fitted Model (MCH = {alpha_high:.3f})')
ax2.set_title(f'High Clustering (Contagion = {contagion_high})', fontsize=20)
ax2.set_xlabel('Mean Imperviousness', fontsize=16)
ax2.legend()
ax2.set_xlim(0, 1)
ax2.tick_params(axis='both', labelsize=15) 


plt.suptitle('Generative Model Linking Clustering to MCH', fontsize=22)
plt.show()

plt.imshow(model_high.grid)
plt.colorbar()
plt.axis('off')
plt.show()

plt.imshow(model_low.grid)
plt.colorbar()
plt.axis('off')
plt.show()

################################################################################

print("Running Full Simulation ")


niters = 1000
contlow = 0
conthigh = 50
n_cores = 10
conts = np.random.uniform(low=contlow,high=conthigh,size=niters)
seeds = np.round(np.random.uniform(low=1,high=15000,size=niters))


# Chunk the contagion factors into 10 pieces
cont_chunks = np.array_split(conts, n_cores)
seed_chunks = np.array_split(seeds, n_cores)

# Function to process a chunk of simulations
def process_chunk(cont_chunk, seed_chunk):
    alphas_chunk = np.zeros(len(cont_chunk))
    mis_chunk = np.zeros(len(cont_chunk))
    
    for i, (cont, seed) in enumerate(zip(cont_chunk, seed_chunk)):
        model = UrbanGrowthModel(contagion_factor=cont)
        means, sds, mi = model.run_simulation(seed=int(seed))  # Ensure seed is int
        alpha = model.analyze_alpha(means, sds)
        alphas_chunk[i] = alpha
        mis_chunk[i] = mi
    
    return alphas_chunk, mis_chunk

# Run 10 parallel jobs (one per chunk)
results = Parallel(n_jobs=n_cores)(
    delayed(process_chunk)(cont_chunk, seed_chunk)
    for cont_chunk, seed_chunk in zip(cont_chunks, seed_chunks)
)

# Unpack and concatenate results
alphas_list, mis_list = zip(*results)
alphas = np.concatenate(alphas_list)
mis = np.concatenate(mis_list)

plt.style.use('default')
plt.scatter(mis,alphas)
plt.xlabel("Moran's I")
plt.ylabel("Mean Conditional Heterogeneity")


import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from sklearn.metrics import r2_score

x = mis
y = alphas

plt.style.use('default')

def exp_func(x, a, b):
    return a * np.exp(b * x)

popt, _ = curve_fit(exp_func, x_fit, y_fit, p0=[100, 0.01])
y_pred = exp_func(x_fit, *popt)
r2 = r2_score(y_fit, y_pred)

plt.figure(figsize=(4.83, 7.5))
plt.scatter(x, y, alpha=0.6,c=conts, cmap='viridis')
plt.plot(
    np.sort(x_fit), 
    exp_func(np.sort(x_fit), *popt), 
    color='k', label='Fitted Curve'
)

# Annotate with equation and R²
a, b = popt
eqn = f"$y = {a:.2f} \cdot exp^{{{b:.2f}*x}}$\n$R^2 = {r2:.2f}$"
plt.text(0.05, 0.85, eqn, transform=plt.gca().transAxes,
         fontsize=15, verticalalignment='top')

# Labels and legend
plt.xlabel("Moran's I",fontsize=20)
plt.ylabel("Mean Conditional Heterogeneity",fontsize=20)
plt.legend()
plt.grid(False)
plt.tight_layout()
plt.tick_params(axis='both', labelsize=15) 
plt.colorbar(label="Contagion Factor")
plt.show()