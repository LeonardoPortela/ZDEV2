*----------------------------------------------------------------------*
*&  Include           RFBELJ10_NACC_HEADER
*&---------------------------------------------------------------------*
* You can use this include to implement your own logic for creating the
* list header of program RFBELJ10_NACC. This include will not be
* overwritten or changed by SAP in the future. However, you have to take
* care that it is handled correctly in SPAU when applying a support
* package or during an upgrade.
*
* This include is contained in the main logic of REFBELJ10_NACC during
* processing of FORM output_top. Therefore you have access to all
* objects which are available at that time.
*
* If you do not want the standard program to write any header
* information you need to set ld_no_standard_header to 'X'. You can
* do so by removing the '*' at the beginning of the following line:
*ld_no_standard_header = 'X'.

* If you do not set ld_no_standard_header to 'X' your header information
* will be written to the list header in addition to the standard header
* information in front of the standard header information. If you would
* like to add your header information below the standard header
* information you need to reomve the '*' in front of the following lines
* and then add your logic afterwards:
*IF noheader IS INITIAL.
*  PERFORM batch-heading(rsbtchh0).
*ELSE.
*  WRITE bhdgd-line2 CENTERED.
*ENDIF.

*There is a special logic for Italian companies regarding the page
*number format in the standard logic. If you want to adjust the output
*of the page number for Italian companies you need to set
*ld_no_page_number_italy to 'X'. You can do so by removing the '*' at
*the beginning of the following line:
*ld_no_page_number_italy = 'X'.
