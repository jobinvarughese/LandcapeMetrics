library(landscapemetrics);library(raster); library(png);library(landscapetools)
setwd("~/Desktop/JOBIN/IISER/4th Sem/JoG/drive-download-20200703T201019Z-001/example/example/")
landscape<-raster("clipped.tif")
check_landscape(landscape)
show_landscape(landscape)
#visualising
show_patches(landscape, class = "all", labels = FALSE)
show_patches(landscape, class = "4", labels = FALSE)
show_patches(landscape, class = "4", directions=4, labels = FALSE)

# calculate the area on patch level
lsm_p_area(landscape)
show_lsm(landscape, what = "lsm_p_area", class = "4", label_lsm = TRUE)

# calculate the circumscribing circle
lsm_p_circle(landscape)
show_lsm(landscape, what = "lsm_p_circle", class = "4", label_lsm = TRUE)

# calculate the perimeter-area ratio
lsm_p_para(landscape)

# calculate the fractal dimension index
lsm_p_frac(landscape)
show_lsm(landscape, what = "lsm_p_frac", class = "4", label_lsm = TRUE)

# calculate the shape index
lsm_p_shape(landscape)
show_lsm(landscape, what = "lsm_p_shape", class = "4", label_lsm = TRUE)

# calculate the contiguity index
lsm_p_contig(landscape)
show_lsm(landscape, what = "lsm_p_contig", class = "4", label_lsm = TRUE)

# calculate the core area
lsm_p_core(landscape)
#visualise cores in class 4 
show_cores(landscape, class = 4, labels = FALSE)
show_lsm(landscape, what = "lsm_p_core", class = "4", label_lsm = TRUE)

#Visualising the cores in all classes
show_cores(
  landscape,
  directions = 4,#The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
  class = "all",
  labels = FALSE,
  nrow = NULL,
  ncol = NULL,
  consider_boundary = FALSE,
  edge_depth = 1
)

# calculate the core area index
lsm_p_cai(landscape)
show_lsm(landscape, what = "lsm_p_cai", class = "4", label_lsm = TRUE)

# calculate the radius of gyration
lsm_p_gyrate(landscape)
show_lsm(landscape, what = "lsm_p_gyrate", class = "4", label_lsm = TRUE)

# calculate the number of core areas
lsm_p_ncore(landscape)
show_lsm(landscape, what = "lsm_p_ncore", class = "4", label_lsm = TRUE)

# calculate the Euclidean nearest-neighbor distance on patch level
enn_patch<-lsm_p_enn(landscape)
show_lsm(landscape, what = "lsm_p_enn", class = "4", label_lsm = TRUE)
write.csv(enn_patch, file="enn_patch.csv")



#class metrics
# calculate total class edge length
lsm_c_te(landscape)
show_lsm(landscape, what = "lsm_c_te", class = "4", label_lsm = TRUE)


# calculate edge density
lsm_c_ed(landscape)

# calculate patch density
lsm_c_pd(landscape)

# calculate Number of patches
lsm_c_np(landscape)

# calculate aggregation index
lsm_c_ai(landscape)

# calculate largest patch index
lsm_c_lpi(landscape)

# calculate  percentage of landscape of class
lsm_c_pland(landscape)

# calculate core area percentage of landscape
lsm_c_cpland(landscape)


#landscape-level metrics
# calculate the total area at landscape level
lsm_l_ta(landscape)

# calculate the patch richness
lsm_l_pr(landscape)

# calculate the Shannon's diversity index
lsm_l_shdi(landscape)

# calculate the Shannon's evenness index
lsm_l_shei(landscape)


# calculate all metrics on patch level
allmetrics_patch<-calculate_lsm(landscape, level = "patch")
write.csv(allmetrics_patch, file="allmetrics_patch.csv")

allmetrics_class<-calculate_lsm(landscape, level = "class")
write.csv(allmetrics_class, file="allmetrics_class.csv")

allmetrics_landscape<-calculate_lsm(landscape, level = "landscape")
write.csv(allmetrics_landscape, file="allmetrics_landscape.csv")



#for calculating co-occurrence matrix
get_adjacencies(landscape)


library(tmap)
temp_data_file = tempfile(fileext = ".tif")
download.file("https://github.com/Nowosad/ent_bp/raw/master/data/landscapes.tif",
              destfile = temp_data_file)
# read the example data
landscapes = brick(temp_data_file)
#visualising the raster
tm_shape(landscapes) +
  tm_raster(palette = c("#FFFF64", "#006400", "#966400", "#BE9600"), 
            style = "cat",
            title = "Land cover category", 
            labels = c("Agriculture", "Forest", "Shrubland", "Grassland")) +
  tm_facets(free.scales = FALSE, nrow = 1) +
  tm_layout(panel.labels = 1:6, legend.outside.position = "bottom")
#One dominating category and second spatially aggregated minor category
#One dominating category and second spatially disaggregated minor category
#One less dominating category and second spatially aggregated minor category
#One less dominating category and second spatially disaggregated minor category
#Four categories, where each one is spatially aggregated
#Four categories, where each one is spatially disaggregated

get_adjacencies(landscapes)
 
#marginal entropy
#represents a diversity (thematic complexity, composition) of spatial categories
mar_ent = lsm_l_ent(landscapes)
mar_ent

#visualising
tm_shape(landscapes) +
  tm_raster(palette = c("#FFFF64", "#006400", "#966400", "#BE9600"), 
            style = "cat",
            title = "Land cover category", 
            labels = c("Agriculture", "Forest", "Shrubland", "Grassland")) +
  tm_facets(free.scales = FALSE, nrow = 1) +
  tm_layout(panel.labels = mar_ent$value, legend.outside.position = "bottom")

#Example data 1 and 2 have one dominating category (low values of marginal entropy), 
#categories are more evenly distributed in example data 3 and 4 (medium values of marginal entropy)
#Example data 5 and 6 have the highest levels of thematic complexity 
#due to the fact of having more unique, evenly distributed categories.


#Mutual Information
#tells how much easier is to predict a category of an adjacent cell 
#if the category of the focus cell is known.
mut_inf = lsm_l_mutinf(landscapes)
mut_inf
#Larger values indicate that the cells of the same category are more aggregated

#visualising
tm_shape(landscapes) +
  tm_raster(palette = c("#FFFF64", "#006400", "#966400", "#BE9600"), 
            style = "cat",
            title = "Land cover category", 
            labels = c("Agriculture", "Forest", "Shrubland", "Grassland")) +
  tm_facets(free.scales = FALSE, nrow = 1) +
  tm_layout(panel.labels = mut_inf$value, legend.outside.position = "bottom")


#Relative Mutual Information
#used to compare spatial data with different number and distribution of categories
#always has a range between 0 and 1 

rel_mut_inf = lsm_l_mutinf(landscapes)$value / lsm_l_ent(landscapes)$value
rel_mut_inf

#the least aggregated (example data 2, 6)
#medium aggregation (example data 4, 5)
#the most aggregated (example data 1 and 3)

#visualising
tm_shape(landscapes) +
  tm_raster(palette = c("#FFFF64", "#006400", "#966400", "#BE9600"), 
            style = "cat",
            title = "Land cover category", 
            labels = c("Agriculture", "Forest", "Shrubland", "Grassland")) +
  tm_facets(free.scales = FALSE, nrow = 1) +
  tm_layout(panel.labels = rel_mut_inf, legend.outside.position = "bottom")

