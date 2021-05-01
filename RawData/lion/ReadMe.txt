SUPPLEMENTAL INFORMATION REGARDING THE BYTES OF THE BIG APPLE LION FILE

As of release 10C, the Department of City Planning will no longer issue a User Guide along with the LION file.  All the information formerly contained in that document has been incorporated into the metadata with the exception of the following:

1.  Locator Files / Geocoding Anomalies

ESRI has developed two customized locator system files (*.lot & *.mat) used for creating Address locators for the LION file.  It is advised that you use these custom files instead of ESRI's out-of-the box address locators due to anomalies in NYC geography.  The files were designed for use with ArcGIS 9.3 as described in "Geocoding with LION.doc" (which is included with the .zip download).  Please note that these files are being provided as a service to our users, but DCP is not responsible for maintaining them.

Also note that while address ranges exist on both roadbed and generic segments of divided roadways (see Layer Files below), geocoding is only available on the roadbed segments (where the addresses actually exist).

There are many street segments in the City in which both odd and even address numbers are found on the same side of the street.  Examples of this are Riverside Drive in Manhattan, and Prospect Park West in Brooklyn.  LION uses the Continuous Parity Indicator field (ConParity) to indicate on which side of the street the addresses physically exist.

ESRI software does not provide a mechanism for properly processing this type of addressing. This could lead to the incorrect graphic placement of individual addresses and the incorrect assignment of addresses to districts such as police precincts and community districts.

To support improved address processing, the LION file contains two records for those segments that have continuous (odd/even) addresses on the same side of the street.  One record has the odd range; the other has the even range. In all other respects, the two records are identical.

The LION file also contains records for various types of Non Addressable Places (such as City Hall and Lincoln Center).  To ensure that non-addressable placenames are geocoded to the correct side of a street segment, the address range fields of the incorrect side of the street will contain a value of "-99999".


2.  Layer Files (.lyr)

The complete LION geodatabase contains a variety of different types of segments, some of which a user may not want to view.  To help facilitate viewing the data, the following layer files are included for your convenience:

A. LION Streets - Generic.lyr:  Displays all generic street features

B. LION Streets - Roadbeds.lyr:  Displays all multi-roadbed street features

C. LION - Generic.lyr:  Displays all non-street features along with generic street features

D. LION - Roadbeds.lyr:  Displays all non-street features along with multi-roadbed street features

E. LION - Street Name Labels.lyr: Displays street name annotation for roadbed geography

F. LION- Street Direction Arrows.lyr (REQUIRES MAPLEX):  Displays traffic directions for roadbeds. This feature requires that the Maplex extension is installed and enabled and the Maplex Label Engine is turned on.  To enable this extension go to:

	1) Tools -> Extensions
	2) Check Maplex extension
	3) Tools -> Customize
	4) Check the labeling toolbar
	5) Once the toolbar is enabled, click on the "labeling" dropdown arrow
	6) Click "Use Maplex label engine"
	7) Click the label manager on the labeling toolbar
	8) Highlight the default label class under "LION - Street Direction Arrows"
	9) Click properties
	10) Under Label position click "orientation"
	11) Check the label alignment box to set the labels to direction of line
	12) Click Ok
	13) Click Apply
    


3.  Understanding Street Names

Some streets may have multiple street names, some of which are valid for the full length of the street (for example, "6 Avenue" and "Avenue of the Americas" in Manhattan) while others apply to only a portion of the full street.  For instance, a portion of "West 110 Street" in Manhattan has the alternative valid street name "Central Park North'; a different portion of "West 110 Street" has the alternative street name "Cathedral Parkway".  The addresses "155 West 110 Street" and "460 Cathedral Parkway" are equivalent; the addresses "460 West 110 Street" and "460 Cathedral Parkway" are also equivalent.  However, "155 Cathedral Parkway" and "460 Central Park North" are not valid addresses, since those street names are not valid for the portions of "West 110 Street" where those respective address numbers are located.

Additionally, there may be multiple valid spellings for a particular street name.  For example, it is acceptable to refer to Adam Clayton Powell Boulevard in Manhattan as Powell Boulevard, Adam Powell Boulevard or A C Powell Boulevard.

Finally, there are some common misspellings of certain street names.  For example, in Brooklyn, Reed St, Richards St and Sandford St are sometimes misspelled Reid St, Richard St, and Sanford St, respectively.

The LION files account for most of these cases through the use of Street Codes and the LGC fields which establish what names are valid for a particular segment.


4.  Join_ID

This is an identification field used to link LION features to the Alternate Names table during a geocoding operation.

Join_ID is the concatenation of Boro/FaceCode/LGC1/LGC2/LGC3/LGC4 and for SAF records, it is Boro/StreetCode/LGC1/LGC2/LGC3/LGC4/SpecAddr.  The alternate street name table is built using all the street names that correspond to the street code and lgc values that are in the Join_ID.  This can be a many-to-many relationship.

A simple example, Absecon Road in Manhattan, has a Join_ID of 1050001000000 .  There is only one LION record (1 segment) with this Join_ID, and the only LGC associated in this Join_ID is LGC 01.  In the altnames table, there is 1 corresponding record with the same Join_ID for Absecon Rd since that is the only valid name associated with that LION segment.

A more complex example is Adam C Powell Blvd.  There are well over 100 LION segments with this name, and 10 different Join_IDs associated with the segments:

1051501040000
1051501040500
1051501040507
1051501040608
1051501040709
11674009000000X
11674010000000X  
11674011000000X
12246007000000X
12246008000000X
 
The first Join_ID for Adam C Powell Blvd is associated with 57 LION segments and has 11 corresponding records in the Altname table.  This represents the street name variants that are valid for LCGs 01 and 04 as indicated in the Join_ID:
 
A C P BLVD
ADAM C POWELL BLVD
A C POWELL BLVD
AC POWELL BLVD
ACP BLVD
ADAM CLAYTON POWELL BLVD
ADAM CLAYTON POWELL JR BLVD
ADAM POWELL BLVD
ADAM POWELL JR BLVD
POWELL BLVD
7 AVE
 
Each of the remaining Join_IDs also have numerous Altname records corresponding to the SND LGC entries.

