# EXTRAAMAS_2025

Software implementations for the paper "Social Explainable AI: What Is It and How to Make It Happen with CIU?", presented at EXTRAAMAS 2025 workshop.

### Installation and Execution

The source code has not been cleaned up and is provided "as-is" for the moment.

All the required packages can be identified from the `library()` calls in the source code. The CIU package used is the one that is found in the repository <https://github.com/KaryFramling/ciu> in May 2025 and it is recommended to install CIU from there, until a new release of CIU is made.

In order to launch the demonstration Shiny app, do source on the files in the following order:

1.  GraphUtilities.R

2.  AmesHousingVocabulary.R

3.  AmesVocabularyTwo.R

4.  Ames_CIU.R

5.  CIUvisNetwork.R

Then do `Run App` for the file Shiny_sXAI.R.

### Remarks

The functionality of this software will be implemented as its own package, or integrated with the CIU package, or a mix of the two. The principles are generic and applicable to any data set, model etc. but the current implementation is hard-coded for the Ames housing data set.
