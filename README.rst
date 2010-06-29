==============
OSGi Utilities
==============

These are some OSGi helper classes written in Scala. The OSGi Utilities include two modules:

- The *bundle parser* reads OSGi manifests and provides a clean interface for accessing bundle attributes like symbolic name, bundle version, required bundles, imported and exported packages
- The *bundle registry* maintains a list of bundles and is able to calculate transitive dependencies.

The main goals of the project are:

- Clean interface
- Independence from OSGi containers like Eclipse Equinox, Apache Felix, ...
- High performance
- Use the power of functional programming
- Accessibility from Scala and Java

Compiling
---------

You need `sbt <http://code.google.com/p/simple-build-tool/>`_ in order to compile the OSGi Utilities. Please follow the `instructions on the sbt wiki <http://code.google.com/p/simple-build-tool/wiki/Setup>`_.

Execute the following command to compile the OSGi Utilities and to run the unit tests::

  sbt update compile test

If everything runs successfully, you may create a .jar library::

  sbt clean package

The library will be located under the ``target`` directory. It can also be used as an OSGi bundle.

Eclipse
.......

The source code includes a Eclipse project file. You may use the `sbt-eclipse-plugin <http://github.com/Gekkio/sbt-eclipse-plugin>`_ to include libraries managed by sbt into the project's classpath.

Usage
-----

Bundle Parser
.............

The following example shows how to use the bundle parser in Scala::

  import de.undercouch.osgiutils.BundleInfo

  val u: java.net.URL = .....; // URL to bundle jar
  val bi = BundleInfo.fromJar(u)
  println("Bundle-Symbolic-Name: " + bi.symbolicName)
  println("Bundle-Version: " + bi.version)
  bi.exportedPackages foreach { ep => println(ep.name) }
  for (name <- bi.name) println(name)

The parser can also be used from Java::

  import static scala.collection.JavaConversions.asList;
  import de.undercouch.osgiutils.BundleInfo;
  import de.undercouch.osgiutils.ExportedPackage;

  java.net.URL u = .....; // URL to bundle jar
  BundleInfo bi = BundleInfo.fromJar(u);
  System.out.println(bi.getSymbolicName());
  System.out.println(bi.getVersion());
  for (ExportedPackage ep : asList(bi.getExportedPackages())) {
      System.out.println(ep.getName());
  }
  if (bi.getName().isDefined()) {
      System.out.println(bi.getName().get());
  }

Bundle Registry
...............

The bundle registry can be used to calculate bundle dependencies::

  import de.undercouch.osgiutils.registry.BundleRegistry
  import BundleRegistry._

  val reg = new BundleRegistry()
  reg.add(bundle1)
  reg.add(bundle2)

  //calculate dependencies of bundle2
  val deps = reg.calculateRequiredBundles(bundle2)
  ...

  //try to resolve all bundles
  val errors = reg.resolveBundles()
  
  //print out errors
  for (e <- errors) e match {
    case MissingRequiredBundle(bundle, r) =>
      println("Bundle " + bundle.symbolicName + " is missing required bundle " + r.symbolicName)
    case MissingImportedPackage(bundle, i) =>
      println("Bundle " + bundle.symbolicName + " is missing imported package " + i.name)
    case MissingFragmentHost(bundle, h) =>
      println("Bundle " + bundle.symbolicName + " is missing fragment host " + h.symbolicName)
  }

License
-------

This work is licensed under the
`MIT license <http://www.opensource.org/licenses/mit-license.php>`_:

Copyright (c) 2010 Michel Kraemer

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
