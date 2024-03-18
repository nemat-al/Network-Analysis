# Simple-Network-Analysis
An interface developed in R language for simple social network analysis tasks.
---

## Index
1. [About the project](#about-the-project)
2. [Used software and Requirments](#used-software-and-requirments)
4. [Results: A senario](#results-a-senario)

---

## About the project
Social Network Analysis aims to detect communities inside a network. Since researchers proposed many approaches, the decision of choosing one method as a task requires experience in the field of social networks. In this project, a defacto standard validation measure (Modularity) was applied to guide any user with no experience in Social Network Analysis in choosing one algorithm for detecting communities. In addition to that, some functionalities were added to the system such as finding the influencer, calculating some centrality measures, and the ability to compare community detection algorithms.

The final product of the project is a program where a user can upload a file of a social network dataset. Through the program, the user can check the recommended algorithms to be applied to their dataset then they can detect different communities using that algorithm. Moreover, users can detect the influencer in the social network, plot the social network and compare the results of different algorithms for detecting the communities.


--- 
## Used Software
The project was developed using R language in R studio enviroment.


---
## Results A senario
In the following we'll walk through a senario case for using the interface:

1. The first step is to upload the dataset file, the program will read it and print main measurements for Social Network, the result as follows in the followin picture.

![alt text](https://github.com/Nemat-Allah-Aloush/Simple-Network-Analysis/blob/562c571a44c54a9748e5afda5f432ea65b7f063a/images/Calculating%20Centraity%20Measurments.png "Calculating Measurements")

2. In the second tab in the interface there is a choice to plot the social network, as we can see in the following image.

![alt text](https://github.com/Nemat-Allah-Aloush/Simple-Network-Analysis/blob/562c571a44c54a9748e5afda5f432ea65b7f063a/images/Plot.png "Plotting the social network").

3. In the third tab we can find wiether each of the algorithms for detecting communities is recommended or not according to the value of modularity measurments. The recommendation results are shown as in the followung photo.

![alt text](https://github.com/Nemat-Allah-Aloush/Simple-Network-Analysis/blob/562c571a44c54a9748e5afda5f432ea65b7f063a/images/Modularity%20and%20recommendation.png "Recommending Algorithms")

4. In the forth tab, a user can choose and apply an algorithm to detect the communities in the social network. Then the interface will show the number of communities and details about the nodes as shown in the following picture. 

![alt text](https://github.com/Nemat-Allah-Aloush/Simple-Network-Analysis/blob/562c571a44c54a9748e5afda5f432ea65b7f063a/images/Detecting%20communities.png "Detecting Communities").

5. In the fift tab a user can compare between the results of two detecting-communities algorithms. The result is as shown in the following picture.

![alt text](https://github.com/Nemat-Allah-Aloush/Simple-Network-Analysis/blob/562c571a44c54a9748e5afda5f432ea65b7f063a/images/Comparing%20different%20algorithms.png "Comparing Algorithms").

6. In the last tab, the user can detect the main influencer in the social network. The result is as shown in the following picture. 

![alt text](https://github.com/Nemat-Allah-Aloush/Simple-Network-Analysis/blob/562c571a44c54a9748e5afda5f432ea65b7f063a/images/Finding%20Influencer.png "finding Influencer")
