# sGit_Project

           
![alt text](https://i.ibb.co/4tVTVjy/Capture-d-cran-2019-10-11-05-16-38.png)
 A Scala-based git-like code source manager
 
 ## Progression
 
 Installation files : Done :white_check_mark:
 
 ### Create 
 sgit init : Done :white_check_mark:
 
 ### Local Changes:
 sgit add : Done :white_check_mark: 
   sgit commit : Done :white_check_mark: 
 sgit status :  Done :white_check_mark: 
 sgit diff:  :interrobang: 
 
  ### Commit history:
 sgit log :   :soon:
      , log -p :   :interrobang:
      , log --stat :   :interrobang:
 
 ### Branches and Tags          
 sgit checkout : :soon:
 sgit branch <branch name>:   Done :white_check_mark:
             -av: Done :white_check_mark:
 sgit tag <branch name>:   Done :white_check_mark:

### Merge & Rebase
sgit merge <branch>:    :soon:
sgit rebase <branch>:  :soon:
sgit rebase -i <commit hash or banch name>  :interrobang:

 
## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Before starting to run the application, the following dependencies must be installed :


sbt version 1.3.2            	  			scala version  2.12.6  


### Installing

A step by step series of examples that tell you how to get a development env running

Import code source

```
> git clone https://github.com/LamhAnissa/sGit-Project.git
```

Execute the first script :  sgit_prepocessing.sh

```
> ./sgit_preprocessing.sh
```


Source the first script :  sgit_prepocessing.sh

```
> source sgit_preprocessing.sh
```

Now you can use the sgit command:
```
> sgit command [option] <arguments>

```
## Running the tests
```
> sbt test
```


## Built With

* https://github.com/pathikrit/better-files -  dependency-free pragmatic thin Scala wrapper around Java NIO.
* https://github.com/scopt/scopt - scopt is a little command line options parsing library.
* https://www.scala-sbt.org/ - Interactive build tool


## Authors

* **Anissa LAMHAOUAR** - *Initial work* Computer science and management - Polytech Montpellier




