echo "Running Analysis"
echo "Dataset1: Wine"
cd wine
echo "Computing values for wine Dataset:"
echo
echo "Computing and Plotting Distributions"
python visualize_features.py
echo "Check out distribution of features for wine Dataset at:"`pwd`
echo "Running Naive Gaussian (GaussianNB)"
python parametric_est.py
echo
echo "Dataset2: Mammographic"
cd ../mammographic
echo "Computing values for mammographic Dataset:"
echo
echo "Computing and Plotting Distributions"
python visualize_features.py
echo "Check out distribution of features for mammographic Dataset at:"`pwd`
echo "Running Naive Gaussian (GaussianNB)"
python parametric_est.py
echo
echo "Analysis Completed"


