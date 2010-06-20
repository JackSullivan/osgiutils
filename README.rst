==============
OSGi Utilities
==============

These are some OSGi helper classes written in Scala.

Compiling
---------

You need `sbt <http://code.google.com/p/simple-build-tool/>`_ in order to compile the OSGi Utilities. Please follow the `instructions on the sbt wiki <http://code.google.com/p/simple-build-tool/wiki/Setup>`_.

Execute the following command to compile the OSGi Utilities and to run the unit tests::

  sbt update compile test

If everything runs successfully, you may create a .jar library::

  sbt clean package

The library will be located under the ``target`` directory. It can also be used as an OSGi bundle.

Usage
-----

The following example shows how to use the OSGi Utilities in Scala::

  import de.undercouch.osgiutils.BundleInfo

  val u: java.net.URL = .....; // URL to bundle jar
  val bi = BundleInfo.fromJar(u)
  println("Bundle-Symbolic-Name: " + bi.symbolicName)
  println("Bundle-Version: " + bi.version)
  bi.exportedPackages foreach { ep => println(ep.name) }
  for (name <- bi.name) println(name)

The library can also be used from Java::

  import de.undercouch.osgiutils.BundleInfo;
  import de.undercouch.osgiutils.ExportedPackage;

  java.net.URL u = .....; // URL to bundle jar
  BundleInfo bi = BundleInfo.fromJar(u);
  System.out.println(bi.getSymbolicName());
  System.out.println(bi.getVersion());
  for (ExportedPackage ep : bi.getExportedPackages()) {
      System.out.println(ep.getName());
  }
  if (bi.getName().isDefined()) {
      System.out.println(bi.getName().get());
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
