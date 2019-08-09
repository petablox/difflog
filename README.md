# Difflog #

## Materials

[Paper](https://www.cis.upenn.edu/~rmukund/pdf/2019-IJCAI.pdf) | 

## Usage
### Toolchain 
- `scala`
- `sbt` 

### How to run
> `sbt -J-Xms4G -J-Xmx8G "run tab2 ./src/test/resources/Path-Train.qd 0.1 400 ./src/test/resources/Path-Test.qd"`

- `-J-Xms4G`/`-J-Xmx8G`: Start the JVM with access to 4GB of memory, and allow it to access no more than 8GB. 
    - Feel free to increase if you have more ram available.
- `tab2`: Generates table 2 of the Difflog paper submission.
- `Path-Train.qd`, `Path-Test.qd`: Training and testing data. 
- `0.1`: Target loss. Stop iterating if the l2 loss drops below this value.
- `400`: Do no more than 400 iterations in any case.
