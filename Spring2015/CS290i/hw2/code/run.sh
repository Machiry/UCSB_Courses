echo "Running Analysis"
echo "Dataset: Seeds"
cd seeds
echo "Computing values for Seeds Dataset:"
echo
echo "Computing and Plotting Features"
python visualize_features.py
echo "Check out distribution of features for Seeds Dataset at:"`pwd`"/data_vis"
echo "Running LDA Technique"
python parametric_est.py
cd ..
echo
echo "Analysis Completed"
