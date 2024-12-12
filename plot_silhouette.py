import matplotlib.pyplot as plt
from sklearn.metrics import silhouette_score, silhouette_samples
import numpy as np

def plot_silhouette_scores(X, cluster_labels):
    """
    Plot the silhouette scores for each sample in the dataset X given clustering labels.

    Parameters:
    - X: Dataset features, array-like of shape (n_samples, n_features).
    - cluster_labels: Cluster labels for each point in X, array-like of shape (n_samples,).
    """
    n_clusters = len(np.unique(cluster_labels))
    silhouette_vals = silhouette_samples(X, cluster_labels)
    silhouette_avg = silhouette_score(X, cluster_labels)
    y_lower = 10

    plt.figure(figsize=(7, 5))
    for i in range(n_clusters):
        ith_cluster_silhouette_values = silhouette_vals[cluster_labels == i]
        ith_cluster_silhouette_values.sort()
        
        size_cluster_i = ith_cluster_silhouette_values.shape[0]
        y_upper = y_lower + size_cluster_i

        color = plt.cm.Spectral(float(i)/n_clusters)
        plt.fill_betweenx(np.arange(y_lower, y_upper), 
                          0, ith_cluster_silhouette_values, 
                          facecolor=color, edgecolor=color)
        plt.text(-0.05, y_lower + 0.5 * size_cluster_i, str(i))
        y_lower = y_upper + 10  # Add 10 for the 0 samples gap between clusters
    
    plt.axvline(x=silhouette_avg, color="red", linestyle="--")
    plt.xlabel("Silhouette coefficient values")
    plt.ylabel("Cluster label")
    plt.title(f"Silhouette plot for the {n_clusters} clusters")
    plt.show()