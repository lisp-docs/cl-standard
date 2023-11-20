Version 15.17R, X3J13/94-101R. 

Fri 12-Aug-1994 6:35pm EDT 

**Programming Language—Common Lisp** 

**A. Appendix** 

Appendix **i**

Version 15.17R, X3J13/94-101R. Fri 12-Aug-1994 6:35pm EDT 

**ii** Programming Language—Common Lisp

Version 15.17R, X3J13/94-101R. 

Fri 12-Aug-1994 6:35pm EDT 

**A.1 Removed Language Features** 

**A.1.1 Requirements for removed and deprecated features** 

For this standard, some features from the language described in *Common Lisp: The Language* have been removed, and others have been deprecated (and will most likely not appear in future Common Lisp standards). Which features were removed and which were deprecated was decided on a case-by-case basis by the X3J13 committee. 

*Conforming implementations* that wish to retain any removed features for compatibility must assure that such compatibility does not interfere with the correct function of *conforming programs*. For example, symbols corresponding to the names of removed functions may not appear in the the COMMON-LISP *package*. (Note, however, that this specification has been devised in such a way that there can be a package named LISP which can contain such symbols.) 

*Conforming implementations* must implement all deprecated features. For a list of deprecated features, see Section 1.8 (Deprecated Language Features). 

**A.1.2 Removed Types** 

The *type* string-char was removed. 

**A.1.3 Removed Operators** 

The functions int-char, char-bits, char-font, make-char, char-bit, set-char-bit, string-char-p, and commonp were removed. 

The *special operator* compiler-let was removed. 

**A.1.4 Removed Argument Conventions** 

The *font* argument to **digit-char** was removed. The *bits* and *font* arguments to **code-char** were removed. 

**A.1.5 Removed Variables** 

The variables char-font-limit, char-bits-limit, char-control-bit, char-meta-bit, char-super-bit, char-hyper-bit, and \*break-on-warnings\* were removed. 

**A.1.6 Removed Reader Syntax** 

The “#,” *reader macro* in *standard syntax* was removed. 

**A.1.7 Packages No Longer Required** 

The *packages* LISP, USER, and SYSTEM are no longer required. It is valid for *packages* with one or more of these names to be provided by a *conforming implementation* as extensions. Appendix **A–1**

Version 15.17R, X3J13/94-101R. Fri 12-Aug-1994 6:35pm EDT 

**A–2** Programming Language—Common Lisp
