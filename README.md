# HaSSQL
[![CircleCI token](https://img.shields.io/circleci/token/734a9a6d01b2f9e068ffedcf869c71a598de6ade/project/github/IITH-SBJoshi/haskell-8/master.svg?logo=circleci&style=for-the-badge)](https://circleci.com/gh/IITH-SBJoshi/haskell-8/tree/master)
[![License](https://img.shields.io/badge/License-BSD--3-informational.svg?style=for-the-badge)](https://github.com/IITH-SBJoshi/haskell-8/blob/master/LICENSE)
[![Inline docs](https://img.shields.io/badge/Haddock-Reference-blue.svg?style=for-the-badge&logo=haskell)](http://104.211.220.36/docs/index.html)

Haskell Simple Structured Query Language is a subset of SQL with a local database integration based on Parsec Library.

## Description
1. **Objective**
        - Understand Functional programming paradigm.
        - Using software technologies like Continous Integration , Unit Testing , Documentation .
2. **Features/Design**
        - Supports ```Select```, ```Create```, ```Insert```, ```Delete``` queries.
        - Supports ```From```, ```Where```, ```ORDER BY``` Clauses.
        - Supports ```Integer```, ```Boolean```, ```String``` Literals.
        - Support aliases in ```Select``` query.
        - A local database backend support to perform queries live.
        - A Command Line Interface for easy Usage.
3. **Team**
	- Shraiysh Gupta (CS17BTECH11050)
	- Puneet Mangla (CS17BTECH11029)
	- Lingam Sai Ramana Reddy (CS17BTECH11022)
	- Hitesh (MA17BTECH11004)
  
## Documentation
  - Present package documentation can be found [here](http://104.211.220.36/docs/index.html).
  - Haddock understands various annotations in the source file, and puts them in the generated docs.
  - Please refer [this link](https://www.haskell.org/haddock/doc/html/markup.html) to know how to comment your source code in order to generate good documentation.
  
 ## Setup
 1. **Installations**
      - Install [Haskell for Linux](https://www.haskell.org/downloads/linux/)
      - Install [stack framework](https://docs.haskellstack.org/en/stable/README/)
 2. **Building and Running tests suites**
      ```
      $ git clone https://github.com/IITH-SBJoshi/haskell-8.git
      $ stack build
      $ stack test
      $ stack run
     ```
 ## Grammar
 ```
 <table_name> or <column_name> -> <Iden>
 <Iden> -> valid identifier except reserved keywords like from, where, order, by, select, insert, into, create, delete etc.
 <datatype> -> INTEGER | STRING | BOOL
 <value> -> <NumLit> | <BoolLit> | <StringLit>
 <BoolLit> -> True | False
 <StringLit> -> valid string in single quotes eg : 'Hello World`
 <NumLit> -> (digit)+
 <value_expression> -> <NumLit> | <BoolLit> | <StringLit> | <Iden> 
 
 ```
 ## Usage/Syntax
 1. **Create table**
      -  ```create table <table_name> (<column_name> <datatype> ,<column_name> <datatype> ... )```
 2. **Insert record**
      - ```insert into <table_name> (<column_name>,<column_name> ...) values (<value>, <value> ...)``` 
      - To add enteries in columns by default : ```insert into <table_name> values (<value>, <value> ...)```
 3. **Delete records**
      - ```delete from <table_name> where <value_expression>```
 4. **Select records**
      - ```select <column_name>, <column_name> ... from <table_name> where <value_expession> order by <value_expession>, <value_expession> ...```
      - To select all columns : ```select * from <table_name> where <value_expession> order by <value_expession>, <value_expession> ...```
      
## Guidelines for contribution :
1. Take open issues and ask for assignment in comment section.
2. **Working on seperate branch**
	- Clone the repository : ```git clone https://github.com/IITH-SBJoshi/haskell-8.git```
	- Create a issue specific branch in cloned repository : ```git checkout -b issue#<issue number>```
	- Run the code by following the steps above
	- You can now start working on your current branch
3. **Testing the changes** : Run the test cases if any: ```stack test```
4. **Commiting the changes**
	- Update ```.gitignore``` if there is any need .
	- To add changes in your working directory : ```git add .```
	- Commit your changes : ```git commit -m "<message>"```
	- Follow a simple commit message guideline eg . ``` Fix <issue_id> : <small description> Author@<your name>```
5. **Pushing the changes**
	- Get current master: `git fetch origin master`
	- Merge master with your branch: `git merge master`
	- Push your changes : ```git push origin <your branch name>:<your branch name>```
	- Make sure that ```Circle CI build``` is passed.
6. **Generating Pull requests :**
	- [Generate a pull request](https://help.github.com/articles/about-pull-requests/) from your ```branch``` branch to ```master``` branch.
	- Give the PR and apt title, and mention `Fixes #<issue_number>` in the comment to link it with the issue.
	- Don't close the issue by your own.
7. **Commenting your Code**
	- Haddock understands various annotations in the source file, and puts them in the generated docs.
  - Please refer [this link](https://www.haskell.org/haddock/doc/html/markup.html) to know how to comment your source code in order to generate good documentation.

 


