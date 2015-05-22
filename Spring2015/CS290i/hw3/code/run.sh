echo
echo "Running Analysis For HW3"
echo
echo "Dataset: Seeds(https://archive.ics.uci.edu/ml/datasets/seeds)"
cd seeds
echo
echo "Running SVM using linear Kernel."
echo
echo "Plotting Validation Results"
python svm.py
echo
echo "Running LDA Using Gradient Descent."
echo
echo "Note: This will take some time (1-2 min) to complete its execution. Sit Tight."
echo
python gradient_descent.py
cd ..
echo
echo "Analysis Completed"
