
![Poster Image](https://github.com/ScottZimmerman/SER2018/blob/master/SER_2018_Poster.png)

# Overview
This walkthrough details the steps for running the jobs outlined in the poster above. There are three types of jobs: Feature distribution, feature importance and feature search jobs. 

# Walkthrough Using the Default Simulation Method
## Step 0: Set up a new scenario
A scenario will contain all of the data for a single set of simulation instructions. Each scenario is contained in a single folder in ./data/ (You may need to create this directory if it doesn't exist).

To set up the directory structure needed for a new scenario, open a command line terminal in the base directory and run the following command, replacing <scenario_name> with a unique name for the scenario:

```
	python new_scenario.py <scenario_name>
```

This script will set up the folder ./data/<scenario_name>. In this directory are two subfolders: ./data/instructions holds all of the instructions for defining the data-generating system, features, and estimators, as well as simulation options. The jobs folder will hold the instructions and results for each specific simulation job. These are detailed below.

## Step 1: Choose Data-generating System (DGS)
We have provided a default method for defining data-generating systems of discrete variables. In this framework, we express a causal system as a Bayesian network based on a directed acyclic graph (DAG). 

To use the standard method, copy all of the files from templates from

./templates/instructions

into 

./data/<scenario_name>/instructions.

Each of these files contains instructions for some aspect of the simulation process. We will begin by modifying the file DAG.json, which defines the DAG structure and variable types. As an example DAG, consider a situation in which A causes Y, and the relationship is confounded by two confounders, W1 and W2. The instructions file for this DAG is provided at templates/instructions/dag.json. 

The document has the following sections:
* "parents": Each key in "parents" is associated with a list of strings of the names of that variable's parents. For example, to define the DAG above, we create four entries in "parents": A, W1, W2 and Y. W1 and W2 have no parents, and so their values are empty lists. A has W1 and W2 as parents, while Y has W1, W2 and A as parents. The resulting subdocument is as follows:

```json
"parents": {
    "A": [
      "W1",
      "W2"
    ],
    "Y": [
      "W1",
      "W2",
      "A"
    ],
    "W2": [],
    "W1": []
}
```

* "posVals": Here we provide the possible values of each of the variables in our DAG. This subdocument must contain one entry for each of the variables in the DAG defined in the "parents" subdocument. For example, for binary variables we provide each variable with a list containing 0 and 1. The resulting subdocument is:

```json
"posVals": {
    "A": [
      0,
      1
    ],
    "Y": [
      0,
      1
    ],
    "W2": [
      0,
      1
    ],
    "W1": [
      0,
      1
    ]
}
```

The final element of the file is "fullTopoOrder", a topological ordering of the variables that defines the order for simulating the data so that each variable is only run after its parents have been run (in this case ["W1","W2","A","Y"] is a valid topological ordering).

## Step 2: Choose Features
See templates/instructions/features for an example set of features. See templates/feature_functions for example code. 

Intiutively speaking, a feature is just something about a data-generating system that can be quantified. Features may be simple, such as the association between two variables, or more complex, such as the amount of confounding between A and Y, or the average treatment effect of A on Y. Mathematically, a feature is any deterministic function of the data-generating system that outputs a real number.

We are concerned with the properties of the system that gives rise to any particular data set, rather than the data set itself. One strategy for measuring a feature of a data-generating system is to generate a very large data set from it, and then apply a function to the resulting data set. Data-generating systems that define the causal relationships between variables can be written so as to allow us to measure either the associational or causal relationships of the system.

Example feature functions for the default data-generating system are available in ./templates/feature_functions.

### Structure of a features instructions subdocument
In order to provide the software with instructions about features we want to measue, we use the file ./instructions/<scenario_name>/features.json. Each element of the "features" subdocument corresponds to a single feature we want to measure. The template shows the structure of the document:

```json
"features": {
  	"FEATURE_NAME":{
  		"handler":"HANDLER_NAME",
  		"options":{},
  		"github":"HANDLER_GITHUB_PATH"
  	},
    "MinCellSize": {
        "handler": "MinCellSize", 
        "options": {}, 
        "github": "MinCellSize.R"
    }, 
    "ATE": {
        "handler": "ATE", 
        "options": {}, 
        "github": "ATE.R"
    }, 
    "OF": {
        "handler": "OF", 
        "options": {}, 
        "github": "OF.R"
    }, 
    "RD": {
        "handler": "RD", 
        "options": {}, 
        "github": "RD.R"
    }, 
    "MinTxProp": {
        "handler": "MinTxProp", 
        "options": {}, 
        "github": "MinTxProp.R"
    }, 
    "Conf": {
        "handler": "Conf", 
        "options": {}, 
        "github": "Conf.R"
    }
}
```
Note: Currently these functions are pulled from the github repo at ScottZimmerman/StudySimulator/Features (this will be changing very soon to also allow the use of local files).

By default, in addition to any features specified in the instructions, the software always calculates four features for the default simulation method: the outcome frequency (OF), crude risk difference (RD), average treatment effect (ATE) and amount of confounding (Conf). The same handler may be used for several different functions by specifying options that are passed to the feature function at runtime (see structure of a feature function document below).

### Structure of a feature function document
A feature function document is an R document that the software imports using "dget". It has the following general structure for the default simulation method:

```R
function(outcome,exposure){
	f <- function(name,options){
		...
		calculate X
		...
		features[[name]] <<- X
	}
	return(f)
}
```

The feature document wraps the feature handler function ("f") in another function that is called when dget imports the handler. Inside of f we calculate X, the value of the feature for the current data-generating system. At the end of f we assign the feature value X to "features": the current list of feature values.

### Defining your own feature functions for the default data-generating system
For the default data-generating system, we have provided utility functions that can be helpful for calculating other features of interest. The example feature functions use these helper functions to calculate the feature values. 

* CP(LHS,RHS): Calculate a conditional probability. LHS and RHS are lists associating variables with their values on the left- and right-hand sides of the bar in the probability expression P(A=a|B=b,C=c), which would be indicated by the lists:
```R
LHS <- list(
	"A"=a
)
```
and
```R
LHS <- list(
	"B"=b,
	"C"=c
)
```

* JP(vals): Calculate a joint probability. "vals" is a list of variables and their values in a jount probability expression, for example P(A=a,B=b) would be indicated by the list"
```R
vals <- list(
	"A"=a,
	"B"=b
)
```

## Step 3: Choose Estimators
Example subdocuments providing instructions for estimators are provided at ./templates/instructions/methods.json and ./templates/instructions/models.json, and example analysis functions are provided in ./templates/analysis_functions.

For the default simulation method, methods.json defines a set of estimation procedures to be run, as well as the options they should be passed. The options include "Q" (outcome model) or "g" (exposure model) entries that refer to elements of models.json. For example, we may define two exposure models and two outcome models in models.json:

```json
{
  "Q": {
    "Q0": "A+W1+W2",
    "Q1": "A+W1+W2+W1*W2"
  },
  "g": {
    "g0": "W1+W2",
    "g1": "W1+W2+W1*W2"
  }
}
``` 

Then, in methods.json, we define the function that runs the estimation, as well as 
```json
{
  "gComp_1": {
    "handler": "gComp",
    "options": {
      "Q": "Q0"
    },
    "github": "gComp_binaryAY.R"
  },
  "IPTW_1": {
    "handler": "IPTW",
    "options": {
      "g": "g0"
    },
    "github": "IPTW_binaryAY.R"
  },
  "unadjusted": {
    "handler": "unadjusted",
    "options": {},
    "github": "unadjusted_binaryAY.R"
  },
  "TMLE_1": {
    "handler": "TMLE",
    "options": {
      "Q": "Q0",
      "g": "g0"
    },
    "github": "TMLE_binaryAY.R"
  }
}
``` 

Note: Currently these functions are pulled from the github ScottZimmerman/StudySimulator/Analysis (this will be changing very soon to also allow the use of local files).

## Step 4: Specify the quantity being estimated (the estimand or "target parameter")
The estimand is a particular feature of the data-generating system that we aim to estimate using the estimators we just specified. In order to determine how well each method performs in estimating the truth (in terms of percent bias and MSE), we must specify how to calculate the truth.

### Target parameter instructions document
In targetParameter.json, we specify the outcome variable name, exposure variable name, and the type of effect to be estimated (only "ATE" is supported currently).

```json
{
  "outcome": "Y",
  "type": "ATE",
  "exposure": "A"
}
```

## Step 5: Specify simulation options
We have provided a sample simulation options subdocument at ./templates/instructions/options.json. For basic use we recommend, however, there are several options that can be modified if desired. The default subdocument is provided below, along with a description of each option.

```json
"options":{
	"nEstimateReps": 100,
	"nFeatureSearchReps_max": 10,
	"featureRepUploadInterval": 20,
	"sampleSize": 1000,
	"probMode": "JD",
	"nFeaturesReps": 20,
	"maxReps_featureDist": 100,
	"startFeatureImportanceIndex": 100
}
```

* nEstimateReps (default = 100): Number of simulated data sets used to calculate measures MSE and percent bias.

* nFeatureSearchReps_max (default = 10): In feature search jobs, defines how many data-generating systems with targeted features should be returned.

* featureRepUploadInterval (default = 20): COMING SOON

* sampleSize (default = 1000): The size of each simulated data set from the data-generating system.

* probMode (default = "JD"): Probability calculation mode. Specify "JD" or "CD".
JD Mode: ("Joint Distribution") Uses the full joint distribution of P(X) to calculate probabilities
CD Mode: ("Conditional Distributions") Uses the factorization P(X) = Product_{i}[P(X_i|Pa(X_i))] to calculate probabilities
JD mode invloves an up-front computational cost of creating the joint distribution and will thus be slow (or impossible) if the full factorial combination of possible variable values is large. CD mode computes exact probabilities through recursive application of the conditional law of total probability and the factorization of joint distributions.

* nFeaturesReps (default = 20): COMING SOON

* maxReps_featureDist (default = 100): The number of data-generating systems created in feature distribution jobs. 

* startFeatureImportanceIndex (default = 100): COMING SOON


## Step 6: Running the simulations
### Creating a job

#### Creating job instructions
To create new job instructions, open a command line terminal in the simulator base directory, and run the following command, replacing <scenario_name> and <job_name> with 

```
	python new_job.py <scenario_name> <job_name>
```

For feature importance jobs will be asked to input whether to begin with a feature distribution job's output, and, if so, which job to use.


#### Types of jobs
The software handles three types of simulation jobs that can be performed in sequence. Example job files for each type are located in ./templates/jobs

* Feature distribution jobs (example file ./templates/jobs/fd.json): In this type of job, we randomly generate many data-generating systems of the structure we have specified in the instructions fileS for the scenario by randomly selecting the data-generating system's parameter values from the allowed ranges. These jobs are useful for examining the the variation in the values of the features that are obtained through the random generation process. The distributions created in this job are generated using rejection sampling to obtain a wide range of possible combinations of feature values. Additionally, since features can be related, this process yields a correlation matrix that tells you the degree of associations between different features, which is valuable information for the other types of jobs. For example, the amount of confounding present in our example DAG above is associated with the effect size. This suggests that in a feature search job, we may have difficulty finding data-generating systems that simultaneously come close to the desired values for both of these features, because once we specify the desired value for one of the features we may restrict the allowable possible values for the other. For example, once you define the crude risk difference, you limit allowable combinations of the true effect and the amount of confounding, since (in our example feature code) Conf = RD-ATE.

The structure of the job file for a feature distribution job is as follows:
```json
{
  "type": "featureDist"
}
```

* Feature importance jobs (example file ./templates/jobs/fi.json): A feature importance job calculates how important each feature is for predicting the best estimator from among the estimators we specified in the instructions file. The "best" estimator is based on a metric of estimator performance, such as MSE or percent bias. 

A feature importance job can either use the output of a feature distribution job, or run on its own. If a feature distribution job is not specified, the parameters of the data-generating system will be randomly chosen to generate a data set (in this case rejection sampling is not used). We generally recommend using a feature distribution job, since the sampling scheme used in feature distribution jobs results in a data set that has a more uniform sample over the possible feature values than is typically obtained by random selection of data-generating system parameters, which provides more a more balanced sample for training the model for predicting feature importance from data-generating system features.

The structure of the job file for a feature importance job is as follows:
```json
{
  "type": "featureImportance",
  "featureDistributionPath":"test_fd"
}
```

Note that specifying "featureDistributionPath" is optional. In this example, "test_fd" is a feature distribution job in the same scenario as the feature importance job.

* Feature search jobs (example file ./templates/jobs/fs.json): 
A feature search jobs attempt to create a collection of data-generating systems for which the values of a set of features are as close as possible to a desired set of target values specified in the job json file. The key result of a feature search job is the portion the cases in which each estimator performs best over a set of data-generating systems corresponding to the feature targets specified in the instructions.

You can conduct a feature search job without having performed feature distribution or feature importance jobs. However, if you are using the software to determine the best estimator for your applied research question we recommend that you choose which features to include in a feature search job based on the output from a feature distribution and feature importance jobs. A feature distribution job tells you which features are correlated. As discussed above, using correlated features in a feature search job may lead to failing to find data-generating systems that match the targets. In this case, you should reconsider whether the combination of feature values you have provided are possible in any data-generating system. Using features that are unimportant, or not using enough important features, can result in a set of causal systems that have inconsistent method performance.

### Running the simulations
To run a simulation job, open a command line terminal in the simulator base directory, and run the following command, replacing <scenario_name> and <job_name> with the names for your scenario and job.

```
	python study_simulator.py <scenario_name> <job_name>
```

For feature importance jobs, run Compile_Results.R and Analyze_results.R after running the job. For feature search jobs, run Plot_FS_Results.R after running the job.

Mac Troubleshooting: If you get the message "'wget' call had nonzero exit status", first [install homebrew](https://brew.sh/) and then use:
```
brew install wget
```
