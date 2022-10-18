# acoustic-monitoring
Code to enable and assist wildlife monitoring efforts that make use of ARUs. I developed this code while conducting passive acoustic monitoring surveys as a part of my capstone project. A bulk of the code will help to handle and validate large detection datasets produced using BirdNET (the primary function for this is called verify_results), but it also includes code that uses the R package monitoR to create detection datasets from pattern matching that can also be passed to the verification/validation functions (this assumes knowledge of monitoR and pattern matching). Additionally, there are functions that can assist in feature extraction and model training/prediction, although these are still pretty raw and finnicky.

The code is being reguarly developed and refined. This is my first github project, and I am very open to collaboration!

