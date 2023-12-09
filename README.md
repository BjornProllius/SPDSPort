# SPDSPort
SPDS
This repository contains a Scala implementation of Synchronized Pushdown Systems, which has been translated from the original project which is hosted at: https://github.com/CodeShield-Security/SPDS

Use as Maven dependency
All projects inside this repository are pushed to GitHub's Maven repository for every release.

To include a dependency from this repository to your poject, you first have to add the repository to your pom file:

<dependencies>
  <dependency>
    <groupId>de.fraunhofer.iem</groupId>
    <artifactId>WPDS</artifactId>
    <version>3.1.1</version>
  </dependency>
</dependencies>
<repositories>
  <repository>
      <id>github</id>
      <url>https://maven.pkg.github.com/CodeShield-Security/SPDS/</url>
  </repository>
</repositories>	
To access the GitHub packages repository, you also need to set up GitHub credentials in your Maven's settings.xml file. Therefore, you need to add a server block with the id github, your username and an access token that has package:read rights to your setting.xml. An in-depth documentation on how to do this can be found here.

Checkout, Build and Install
To build and install SPDS into your local repository, run

mvn clean install -DskipTests

in the root directory of this git repository. If you do not want to skip the test cases, remove the last flag.

Examples
Boomerang code examples can be found here. Code examples for IDEal are given here.

Notes on the Test Cases
The projects Boomerang and IDEal contain JUnit test suites. As for JUnit, the test methods are annotated with @Test and can be run as normal JUnit tests. However, these methods are not executed but only statically analyzed. When one executes the JUnit tests, the test method bodies are supplied as input to Soot and a static analysis is triggered. All this happens in JUnit's @Before test time. The test method itself is never run, may throw NullPointerExceptions or may not even terminate.

If the static analysis succeeded, JUnit will officially label the test method as skipped. However, the test will not be labeled as Error or Failure. Even though the test was skipped, it succeeded. Note, JUnit outputs a message:

org.junit.AssumptionViolatedException: got: <false>, expected: is <true>

This is ok! The test passed!
