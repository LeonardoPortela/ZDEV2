report zrm07mlbd_2 message-id m7.

*INITIALIZATION.                                              "3203834->
*  "{ begin of MGV_GENERATED_RM07MLBD001
*  DATA: lt_sel_dtel TYPE  rsseldtel OCCURS 0,
*        ls_sel_dtel TYPE  rsseldtel.
*
*  ls_sel_dtel-name = 'MFRPN'.
*  ls_sel_dtel-kind = 'S'.
*  ls_sel_dtel-datenelment = 'MFRPN'.
*  APPEND ls_sel_dtel TO lt_sel_dtel.
*
*  CALL FUNCTION 'SELECTION_TEXTS_MODIFY_DTEL'
*    EXPORTING
*      program                     = sy-repid
*    TABLES
*      sel_dtel                    = lt_sel_dtel
*    EXCEPTIONS
*      program_not_found           = 1
*      program_cannot_be_generated = 2
*      OTHERS                      = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*  "{ end of MGV_GENERATED_RM07MLBD001                        "3203834<-
*ENHANCEMENT-POINT rm07mlbd_g4 SPOTS es_rm07mlbd STATIC.
*ENHANCEMENT-POINT rm07mlbd_g5 SPOTS es_rm07mlbd.
*ENHANCEMENT-POINT rm07mlbd_g6 SPOTS es_rm07mlbd STATIC.
*ENHANCEMENT-POINT rm07mlbd_g7 SPOTS es_rm07mlbd.

************************************************************************
*     REPORT RM07MLBD   (Transaktionscode MB5B)                        *
************************************************************************

* new function April 2012 EH                                "n1710850
* - Installed ability for secondary database connection     "n1710850
*   configuration via Tx HDBC                               "n1710850

* improvement September 2011 MS                             "n1558298
* - MSEG was enhanced by most important fields of MKPF      "n1558298
*   These fields are getting redundantly filled by each GM  "n1558298
* - MB5B was improved to select BUDAT from these fields of  "n1558298
*   table MSEG instead of MKPF. Due to this logic the       "n1558298
*   expensive MSEG-MKPF join can be improved exorbitantly   "n1558298

* correction March 2011 PR                                  "n1560727
* Ensure that valuation stock option can also be used for   "n1560727
* individual valuation types that only exist in EBEW/QBEW   "n1560727

* correction Sep 2010 EH                                    "n1509405
* Removed filter function in append lists as it is no more  "n1509405
* supported by the ALV                                      "n1509405

* correction Nov. 2009 PR
* Batches where a MCHB entry no longer exists but a MCHA    "n1404822
* entry is available are not correctly considered when      "n1404822
* flag 'XNOMCHB' is set                                     "n1404822

* correction Oct. 2009 BS                                   "n1399766
* In case you use a layout with filter criteria for         "n1399766
* 'Opening Stock' or 'Closing Stock' you will get a         "n1399766
* short dump.                                               "n1399766

* correction Sept. 2009 PR                                  "n1390970
* When the *Totals only' settings is used and the report is "n1390970
* run for storage/batch stock, the plant is not considered  "n1390970
* when creating the detail document list                    "n1390970

* correction Apr. 2009 MS                                   "n1333069
* The report texts t-096, t-099 ,t-100 and t-103 are not    "n1333069
* properly displayed in all languages. Same with tooltips   "n1333069

* correction Oct. 2008 TW                                   "n1265674
* for active ingredient materials MB5B should not display   "n1265674
* the 141 and 142 movements for the selection valuated      "n1265674
* stock to avoid wrong beginning stock amount.              "n1265674

* correction Nov. 2007 MS                                   "n1117067
* The dates are displayed in the wrong format in the output "n1117067
* list header. No conversion was done.                      "n1117067

* correction June 2007 MS                                   "n1064332
* fields "Date to" and "Date from" are wrong displayed in   "n1064332
* layout change popup in the mode                           "n1064332
* "totals only - hierarchical list"                         "n1064332

* correction Jan. 2007 MS                                   "n1018717
* convert unit of measurement from internal to external     "n1018717
* format. This was wrong displayed in header of output list "n1018717

* correction Nov. 2006 TW                                   "n999530
* plant description should appear behind plant number but   "n999530
* nevertheless the plant description should not be vissible "n999530
* for all possible selection combinations the transaction   "n999530
* MB5L could be started for.                                "n999530

* correction June 2006 MM                                   "n951316
* - do not allow to form sums for the columns quantity and  "n951316
*   value in the mode "totals only - hierarchical list"     "n951316

* correction Feb. 2006 MM                                   "n921165
* - improve performance processing internal tables          "n921165
*                                                           "n921165
* - improve performance of the access database tables MKPF  "n921165
*   and MSEG using database specific hints for the database "n921165
*   systems :                                               "n921165
*   - DB2 and ORACLE :                                      "n921165
*     - one SELECT command with DBI hints                   "n921165
*   - DB6, Informix, MaxDB, MSSQL :                         "n921165
*     - 3 SELECT commands who could be choosen using 3 new  "n921165
*       related parameters pa_dbstd, pa_dbmat, pa_dbdat     "n921165
*                                                           "n921165
* - some dynamic BREAK-POINT from checkpoint-group          "n921165
*   MMIM_REP_MB5B implemented, use transaction SAAB         "n921165

* correction Nov. 2005 MM                                   "n890109
* allow the inter active functions 'Specify drill-down'     "n890109
* and 'Choose' from the menu 'Settings -> Summation levels' "n890109
* These functions are activated by default. The flag        "n890109
* "g_cust_sum_levels" in include "RM07MLBD_CUST_FIELDS"     "n890109
* allows to activate or inactivate these functions          "n890109

* correction Sept 2005 MM                                   "n878753
* reports runs although the user has no authorization       "n878753

* correction Aug. 2005 MM                                   "n856424
* - the start and end dates were shown incorrectly in the   "n856424
*   headlines in the mode valuated stock                    "n856424
* - the fields "entry time", "entry date", and "User" are   "n856424
*   are not filled filled for price change documents        "n856424

* MB5B improved regarding accessibilty                      "n773673
* - the top_of_page headlines are now printed with the
*   ALV tools
* - the non ALV sum function was replaced by a hierarchical
*   and a non-hierarchical ALV lists
* - double click in the sum lists shows the normal list for
*   the selected material
* - the function special processing was changed : This
*   function depended on the retail system settings or
*   a modifiaction. Now this function depends on the found
*   MM documents

* correction June 2004 MM                                   "n747306
* wrong the assignment of the MM and FI documents for data  "n747306
* constellation : n MM doc items --> 1 FI doc item          "n747306

* ABAP-Preprocessor removed                                 "n599218 A
* - this version is for release 4.6C and higher             "n599218 A
* - process database table OBEW always                      "n599218 A
* - IS-OIL specific functions :                             "n599218 A
*   - define IS-OIL workings fields                         "n599218 A
*   - transport and process these fields only when          "n599218 A
*     structure MSEG comprise these fields                  "n599218 A

* Improvements :                       Dec. 2003 MM         "n599218
* - print the page numbers                                  "n599218
*                                                           "n599218
* - send warnings and error messages only when report is    "n599218
*   launched / advoid warnings when user changes entries on "n599218
*   the selection screen                                    "n599218
* - send warning M7 689 when user does not restrict the     "n599218
*   database in dialog or print mode                        "n599218
* - send warning M7 393 when user deletes the initial       "n599218
*   display variant                                         "n599218
*                                                           "n599218
* - allow to process the fields MAT_KDAUF, MAT_KDPOS, and   "n599218
*   MAT_PSPNR from release 4.5B and higher                  "n599218
*                                                           "n599218
* - show the current activity and the progress              "n599218
*                                                           "n599218
* - error message 'programmfehler' improved                 "n599218
*                                                           "n599218
* - new categories for scope of list                        "n599218
*                                                           "n599218
* - use function module for database commit for the update  "n599218
*   of the parameters in table ESDUS. This allows to record "n599218
*   this transaction for a batch input session using        "n599218
*   transaction SHDB                                        "n599218
*                                                           "n599218
* - reset the entries for plant when valuation level is     "n599218
*   is company code and mode is valuated stock              "n599218
*                                                           "n599218
* - enable this report to run in the webreporting mode      "n599218

* Dec. 2002 MM                                              "n571473
* the definition of the selection screen moved from include "n571473
* RM07MLBP into this report                                 "n571473

* Sept 2002 MM                                              "n555246
* log function tax auditor                                  "n555246

* note 547170 :                              August 2002 MM "n547170
* - representation of tied empties improved                 "n547170
*   active this function automatically in retail systems    "n547170
* - FORM routines without preprocessor commands and without "n547170
*   text elements moved to the new include reports          "n547170
*   RM07MLBD_FORM_01and RM07MLBD_FORM_02                    "n547170
* - the function module FI_CHECK_DATE of note 486477 will   "n547170
*   be processed when it exists                             "n547170
* - function and documentation of parameter XONUL improved  "n547170
* - display MM documents with MIGO or MB03 depending from   "n547170
*   the release                                             "n547170
* - get and save the parameters per user in dialog mode     "n547170
*   only in release >= 4.6                                  "n547170

* the following items were improved with note 497992        "n497992
*
* - wrong results when remaining BSIM entries contain       "n497992
*   an other quantity unit as material master MEINS         "n497992
* - improve check FI summarization                          "n497992
* - the messages M7 390, M7 391, and M7 392                 "n497992
* - definition of field g_f_repid for all releases          "n497992
*
* - incomplete key for access of internal table IT134M      "n497992
*   causes wrong plant selection                            "n497992
* - the function "no reversal movement" did not surpress    "n497992
*   the original movements; fields "SJAHR" was moved from   "n497992
*   from report RM07MLBD_CUST_FIELDS to RM07MLBD            "n497992
* - process valuated subcontractor stock from database      "n497992
*   table OBEW if it exists                                 "n497992
* - if FI summarization is active process warning M7 390    "n497992
*   for stock type = valuated stock                         "n497992
* - the user wants to restrict the movement type : process  "n497992
*   warning M7 391                                          "n497992
* - the user wants to surpress the reversal movements :     "n497992
*   process warning M7 392                                  "n497992
* - consider special gain/loss-handling of IS-OIL           "n497992
* - automatic insert of field WAERS currency key into the   "n497992
*   field catalogue :                                       "n497992
*   - at least one ref. field is active -> WAERS active     "n497992
*   - all reference fields are hidden   -> WAERS hidden     "n497992
* - the length of sum fields for values was increased       "n497992

* - customizing for the selection of remaining BSIM entries "n497992
* - customizing for the processing of tied empties          "n497992

* separate time depending authorization for tax auditor     "n486477

* additional fields are displayed in wrong format           "n480130

* report RM07MLBD and its includes improved  Nov 2001       "n451923
* - merging FI doc number into table G_T_MSEG_LEAN improved "n451923
* - handling of the short texts improved                    "n451923
* - some types and data definitions -> include RM07MLDD     "n451923
*----------------------------------------------------------------------*
* error for split valuation and valuated special stock      "n450764
*----------------------------------------------------------------------*
* process 'goods receipt/issue slip' as hidden field        "n450596
*----------------------------------------------------------------------*
* error at start date : material without stock has value    "n443935
*----------------------------------------------------------------------*
* wrong results for docs with customer consignment "W"      "n435403
*----------------------------------------------------------------------*
* error during data selection for plants                    "n433765
*----------------------------------------------------------------------*
* report RM07MLBD and its includes improved  May 10th, 2001 "n400992
*----------------------------------------------------------------------*
* !!! IMPORTANT : DO NOT CHANGE OR DELETE THE COMMENT LINES !!!        *
*----------------------------------------------------------------------*
*
* - consider the material number during looking for FI documents
*
* - field "g_cust_color" in include report "RM07MLBD_CUST_FIELDS"
*   allows the customer to activate or inactivate the colors in the
*   lines with the documents.
*
* - error during calcuation of start stock for special stock "M"
*
* - valuted stocks required : no documents found ? continue and
*   process empty document table
*
* - the length of sum fields for quantities has been increased
*   to advoid decimal overflow
*
* - table ORGAN is replaced by G_T_ORGAN
*   - it is filled by the following ways :
*     - at process time at selection screen if the
*       user wants the selection via cc or plant
*     - otherwise after the database selection of the stock
*       tables
*   - it contains less data fields
*   - it contains all entries twice, for binary search
*     with plant or valuation area
*
* - selection of databases MKPF and MSEG in one SELECT
*   command with an inner JOIN
*
* - authority checks after the database selections
*
* - result of database selection from the both database tables
*   MSEG and MKPF in working table G_F_MSEG_LEAN instead of
*   the tables IMSEG and IMKPF
*
* - the number of processed data fields was reduced
* - the user has the possibility to increase the number of
*   the processed fields deleting the '*' in the types-command
*   in include report RM07MLBD_CUST_FIELDS
*
* - the creation of the field catalog for the ALV considers
*   only the fields of structure G_S_MSEG_LEAN
*
* - the new table G_T_BELEG contains the results for the ALV.
*   the number of fields of table G_T_BELEG corresponds with
*   the number of fields of table G_T_MSEG_LEAN.
*
* - the functions "define breakdown" and "choose" are inactivated
*   in the menue, because they are are not carried out correctly
*   in all blocks of the list
*
************************************************************************
*     Anzeige der Materialbestände in einem Zeitintervall              *
************************************************************************
*  Der Report gliedert sich im wesentlichen in folgende Verarbeitungs- *
*  blöcke:                                                             *
*  1) Definition des Einstiegsbildes und Vorbelegung einzelner         *
*     Selektionsfelder, sowie Prüfung der eingegebenen Selektions-     *
*     parameter und Berechtigungsprüfung                               *
*  2) Lesen der aktuellen Bestandswerte                                *
*  3) Lesen und Verarbeiten der Materialbelege                         *
*  4) Berechnung der Bestandswerte zu den vorgegebenen Datümern        *
*  5) Ausgabe der Bestände und Materialbelege                          *
************************************************************************

type-pools:  imrep,                   " Typen Bestandsführungsreporting
             slis.                    " Typen Listviewer

* allow the interactions 'Specify drill-down' etc..         "n890109
type-pools : kkblo.          "Korrektur ALV                 "n890109

include:  zrm07mldd_2.     " reportspezifische Datendefinitionen

* controls the "expensive" checks like authorization, etc.  "n878753
data : g_flag_launched(01)   type  c.                       "n878753

* working fields for the performance improvements           "n921165
data : g_flag_db_parameters(01) type  c,                    "n921165
       g_f_database(03)         type  c,                    "n921165
                                                            "n921165
       g_cnt_radio              type  i,                    "n921165
       g_cnt_error_dba          type  i.                    "n921165
                                                            "n921165
data : g_tabix_set   type  sy-tabix,                        "n921165
       g_flag_sorted type  c.                               "n921165
                                                            "n921165
* these flags allow to ignore multiple stops at dynamic     "n921165
* BREAK-POINTs in LOOPs                                     "n921165
data : begin of g_flag_break,                               "n921165
         b1(01) type  c   value 'X',                        "n921165
         b2(01) type  c   value 'X',                        "n921165
         b3(01) type  c   value 'X',                        "n921165
         b4(01) type  c   value 'X',                        "n921165
         b5(01) type  c   value 'X',                        "n921165
         b6(01) type  c   value 'X',                        "n921165
         b7(01) type  c   value 'X',                        "n921165
         b8(01) type  c   value 'X',                        "n921165
       end of g_flag_break.                                 "n921165

data: d_from(10) type c,                                    "n1117067
      d_to(10)   type c.                                    "n1117067

data:  g_f_msegex_act(1) type c.                            "n1558298

*----------------- note 1481757 typedefinition for error-messages-------*

types: begin of mbarc_message,                              "n1481757
         msgid like sy-msgid,                               "n1481757
         msgno like sy-msgno,                               "n1481757
         msgv1 like sy-msgv1,                               "n1481757
         msgv2 like sy-msgv2,                               "n1481757
         msgv3 like sy-msgv3,                               "n1481757
         msgv4 like sy-msgv4,                               "n1481757
       end of mbarc_message.                                "n1481757
types: mbarc_message_tab type mbarc_message occurs 0.       "n1481757
data: archive_messages  type mbarc_message_tab with header line, "n1481757
      g_flag_answer(01) type  c.                            "n1481757

*----------end of note 1481757 typedefinition for error-messages------*

data: gv_switch_ehp6ru type boole_d.

data: dbcon        type dbcon_name,                         "n1710850
      dbcon_active type dbcon_name.                         "n1710850
constants: c_hdb_dbcon_get type funcname value 'MM_HDB_DBCON_GET', "n1710850
           c_hdb_subappl   type program  value 'MB5B'.      "n1710850


data: gv_ui_opt_active type abap_bool.                      "1790231

data: gv_where_clause   type string,                        "n_1899544
      gv_not_authorized type string.                        "n_1899544

data: gv_run_mode type char1.                               "3116194

*-----------------------------------------------------------"n571473
* define the selection screen here                          "n571473
*-----------------------------------------------------------"n571473
selection-screen begin of block database-selection
  with frame title text-001.
*  Text-001: Datenbankabgrenzungen
  select-options: matnr for mard-matnr memory id mat
                                       matchcode object mat1.


  "{ Begin ENHO AD_MPN_PUR2_RM07MLBD IS-AD-MPN-MD AD_MPN }
  if cl_immpn_cust=>check_mpn_active( ) = abap_true.
* DI A&D MPN
    select-options:
                    mfrpn for mara-mfrpn memory id mpn
                                  matchcode object htn.
  endif.
  "{ End ENHO AD_MPN_PUR2_RM07MLBD IS-AD-MPN-MD AD_MPN }

*ENHANCEMENT-POINT rm07mlbd_01 SPOTS es_rm07mlbd STATIC.
  select-options:
                  bukrs for t001-bukrs  memory id buk,
                  hkont for bseg-hkont  modif  id hkt,
                  werks for t001w-werks memory id wrk,
                  lgort for t001l-lgort,
                  charg for mchb-charg,
                  bwtar for mbew-bwtar,
                  bwart for mseg-bwart.
  parameters sobkz like mseg-sobkz.
  selection-screen skip.
  select-options: datum for mkpf-budat no-extension.
*  Datumsintervall für Selektion
selection-screen end of block database-selection.

*----------------------------------------------------------------------*

selection-screen begin of block bestandsart
  with frame title text-002.
*  Text-002: Bestandsart

  selection-screen begin of line.
    selection-screen position 33.
    parameters lgbst like am07m-lgbst radiobutton group bart default 'X'.
    selection-screen comment 40(50) text-010 for field lgbst.
*  Text-010: Lagerort-/Chargenbestand
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen position 33.
    parameters bwbst like am07m-bwbst radiobutton group bart.
    selection-screen comment 40(50) text-011 for field bwbst.
*  Text-011: bewerteter Bestand
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen position 33.
    parameters sbbst like am07m-sbbst radiobutton group bart.
    selection-screen comment 40(50) text-012 for field sbbst.
*  Text-012: Sonderbestand
  selection-screen end of line.

selection-screen end of block bestandsart.

*----------------------------------------------------------------------*

* improved definition of parameters for scope of list       "n599218

selection-screen begin of block listumfang
  with frame title text-003.  "Listumfang

* the following 3 parameters became obsolete do not use     "n599218
* anymor. They are still here to inform the user about      "n599218
* that he is using old variants or SUBMIT commands          "n599218
  parameters :                                              "n599218
    xonul  like am07m-xonul            no-display,          "n599218
    xvbst  like am07m-xvbst            no-display,          "n599218
    xnvbst like am07m-xnvbs            no-display.          "n599218

* 7 new categories for the scope of list                    "n599218
*                                                           "n599218
* cat. I docs I stock on   I    I stock on I Parameter      "n599218
*      I      I start date I    I end date I                "n599218
* -----+------+------------+----+----------+----------      "n599218
*  1   I yes  I =  zero    I =  I =  zero  I pa_wdzer       "n599218
*  2   I yes  I =  zero    I <> I <> zero  I pa_wdzew       "n599218
*  3   I yes  I <> zero    I <> I =  zero  I pa_wdwiz       "n599218
*  4   I yes  I <> zero    I <> I <> zero  I pa_wdwuw       "n599218
*  5   I yes  I <> zero    I =  I <> zero  I pa_wdwew       "n599218
*      I      I            I    I          I                "n599218
*  6   I no   I =  zero    I =  I =  zero  I pa_ndzer       "n599218
*  7   I no   I <> zero    I =  I <> zero  I pa_ndsto       "n599218
*                                                           "n599218
* definition of the pushbutton : show or hide the following "n599218
* parameters for the scope of list                          "n599218
  selection-screen pushbutton /1(20) pb_liu                 "n599218
    user-command liu.                                       "n599218
                                                            "n599218
* text line : materials with movements                      "n599218
  selection-screen begin of line.                           "n599218
    selection-screen comment 1(55) text-072                 "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
* with movements / start = zero  =  end = zero              "n599218
*  1   I yes  I =  zero    I =  I =  zero  I pa_wdzer       "n599218
  selection-screen begin of line.                           "n599218
    selection-screen position 33.                           "n599218
    parameters : pa_wdzer    like am07m-mb5b_xonul          "n599218
    modif id liu.                                           "n599218
*   text-083 : no opening stock ; no closing stock          "n599218
    selection-screen comment 40(70) text-083                "n599218
      for field pa_wdzer                                    "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
* with movements / start = zero  =  end <> zero             "n599218
*  2   I yes  I =  zero    I <> I <> zero  I pa_wdzew       "n599218
  selection-screen begin of line.                           "n599218
    selection-screen position 33.                           "n599218
    parameters : pa_wdzew    like am07m-mb5b_xonul          "n599218
    modif id liu.                                           "n599218
*   text-084 : no opening stock ; with closing stock        "n599218
    selection-screen comment 40(70) text-084                "n599218
      for field pa_wdzew                                    "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
* with movements / start stock <> 0 / end stock = 0         "n599218
*  3   I yes  I <> zero    I <> I =  zero  I pa_wdwiz       "n599218
  selection-screen begin of line.                           "n599218
    selection-screen position 33.                           "n599218
    parameters : pa_wdwiz    like am07m-mb5b_xonul          "n599218
    modif id liu.                                           "n599218
*   text-085 : with opening stock ; no closing stock        "n599218
    selection-screen comment 40(70) text-085                "n599218
      for field pa_wdwiz                                    "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
* with movements / with start and end stocks / different    "n599218
*  4   I yes  I <> zero    I <> I <> zero  I pa_wdwuw       "n599218
  selection-screen begin of line.                           "n599218
    selection-screen position 33.                           "n599218
    parameters : pa_wdwuw    like am07m-mb5b_xonul          "n599218
    modif id liu.                                           "n599218
*   with opening stock ; with closing stock ; changed       "n599218
    selection-screen comment 40(70) text-086                "n599218
      for field pa_wdwuw                                    "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
* with movements / with start and end stock / equal         "n599218
*  5   I yes  I <> zero    I =  I <> zero  I pa_wdwew       "n599218
  selection-screen begin of line.                           "n599218
    selection-screen position 33.                           "n599218
    parameters : pa_wdwew    like am07m-mb5b_xonul          "n599218
    modif id liu.                                           "n599218
*   with opening stock ; with closing stock ; non-changed   "n599218
    selection-screen comment 40(70) text-087                "n599218
      for field pa_wdwew                                    "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
* text line : materials without movements                   "n599218
  selection-screen begin of line.                           "n599218
    selection-screen comment 1(55) text-073                 "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
* materials without movements / stocks = zero               "n599218
*  6   I no   I =  zero    I =  I =  zero  I pa_ndzer       "n599218
  selection-screen begin of line.                           "n599218
    selection-screen position 33.                           "n599218
    parameters : pa_ndzer    like am07m-mb5b_xonul          "n599218
    modif id liu.                                           "n599218
*   text-083 : no opening stock ; no closing stock          "n599218
    selection-screen comment 40(70) text-083                "n599218
      for field pa_ndzer                                    "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
* materials without movements / with start or end stock     "n599218
*  7   I no   I <> zero    I =  I <> zero  I pa_ndsto       "n599218
  selection-screen begin of line.                           "n599218
    selection-screen position 33.                           "n599218
    parameters : pa_ndsto    like am07m-mb5b_xonul          "n599218
    modif id liu.                                           "n599218
*   with opening stock ; with closing stock ; non-changed   "n599218
    selection-screen comment 40(70) text-087                "n599218
      for field pa_ndsto                                    "n599218
      modif id liu.                                         "n599218
  selection-screen end of line.                             "n599218
                                                            "n599218
selection-screen end of block listumfang.

*----------------------------------------------------------------------*

selection-screen begin of block einstellungen
  with frame title text-068.  "Settings
*--CS2020001127 - Jaime Tassoni - 04.11.2020 - inicio
  selection-screen begin of line.
    selection-screen position 1.
    parameters p_xlgort      like am07m-xsum    default 'X'.
    selection-screen comment 4(60) text-142 for field p_xlgort.
  selection-screen end of line.
*--CS2020001127 - Jaime Tassoni - 04.11.2020 - fim

* parameter for totals only - hierseq. list
* corresponding display variant
  selection-screen begin of line.
    selection-screen position 33.
    parameters xsum          like am07m-xsum.
    selection-screen comment 40(60) text-090 for field xsum.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 1(30) text-091 for field pa_suvar.
    selection-screen position 33.
    parameters: pa_suvar like disvariant-variant.
  selection-screen end of line.

* parameter for totals only - flat list + corresponding display variant
  selection-screen begin of line.
    selection-screen position 33.
    parameters pa_sumfl like am07m-xsum.
    selection-screen comment 40(60) text-092 for field pa_sumfl.
  selection-screen end of line.

  selection-screen begin of line.                           "1790231
    selection-screen position 33.                           "1790231
    parameters: p_grid type mb_opt_alv_grid_ui              "1790231
    modif id opt user-command opt.                          "1790231
    selection-screen comment 40(50) for field p_grid        "1790231
      modif id opt.                                         "1790231
  selection-screen end of line.                             "1790231

  selection-screen begin of line.
    selection-screen comment 1(30) text-091 for field pa_sflva.
    selection-screen position 33.
    parameters: pa_sflva like disvariant-variant.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen position 33.
    parameters xchar like am07m-xchrg.
    selection-screen comment 40(50) text-015 for field xchar.
*  Text-015: nur chargenpflichtige Materialien
*  Das Kennzeichen 'xchar' bestimmt die Art der Listausgabe entweder
*  auf Material- oder Chargenebene.
  selection-screen end of line.

  selection-screen begin of line.                           "838360_v
    selection-screen position 33.
    parameters xnomchb like am07m-mb5b_xnomchb.
    selection-screen comment 40(50) text-089 for field xnomchb.
*  Text-089: Auch Chargen ohne Bestandssegment
  selection-screen end of line.                             "838360_^

* the function "No reversal movements" is only         "n571473
* available from relaese 4.5B and higher               "n571473
* ( TEXT-026 : No reversal movements )                 "n571473
  selection-screen begin of line.                           "n571473
    selection-screen position 33.                           "n571473
    parameters nosto like am07m-nosto.                      "n571473
    selection-screen comment 40(50) text-026                "n571473
      for field nosto.                                      "n571473
  selection-screen end of line.                             "n571473

selection-screen end of block einstellungen.

*----------------------------------------------------------------------*

selection-screen begin of block liste with frame title text-040.
  parameters: p_vari like disvariant-variant.
selection-screen end of block liste.

*----------------------------------------------------------------------*

* with these new parameters allow the user to determine     "n921165
* the best database access; these parameters will appear    "n921165
* only when the installed database system is :              "n921165
* DB6, Informix, or MaxDB                                   "n921165
                                                            "n921165
* define database access for best runtime                   "n921165
selection-screen begin of block db                          "n921165
  with frame title text-111.                                "n921165
                                                            "n921165
* Database determines best access                           "n921165
  selection-screen : begin of line.                         "n921165
    selection-screen position 33.
    parameters : pa_dbstd    like  am07m-xselk              "n921165
                             modif id dba                   "n921165
                             default 'X'                    "n921165
    radiobutton group db.                                   "n921165
    selection-screen : comment 40(70) text-112              "n921165
      for field pa_dbstd                                    "n921165
      modif id dba.                                         "n921165
  selection-screen : end of line.                           "n921165
                                                            "n921165
* Access via Material number                                "n921165
  selection-screen : begin of line.                         "n921165
    selection-screen position 33.
    parameters : pa_dbmat    like  am07m-xselk              "n921165
                             modif id dba                   "n921165
    radiobutton group db.                                   "n921165
    selection-screen : comment 40(70) text-113              "n921165
      for field pa_dbmat                                    "n921165
      modif id dba.                                         "n921165
  selection-screen : end of line.                           "n921165
                                                            "n921165
* Access via Posting Date                                   "n921165
  selection-screen : begin of line.                         "n921165
    selection-screen position 33.
    parameters : pa_dbdat    like  am07m-xselk              "n921165
                             modif id dba                   "n921165
    radiobutton group db.                                   "n921165
    selection-screen : comment 40(70) text-114              "n921165
      for field pa_dbdat                                    "n921165
      modif id dba.                                         "n921165
  selection-screen : end of line.                           "n921165
                                                            "n921165
selection-screen end of block db.                           "n921165

*------------------------ begin of note 1481757 ------------------------*
*---------- selection-sreen for archive --------------------------------*

selection-screen begin of block arch with frame title text-131. "n1481757
                                                            "n1481757
* create checkbox on the screen                                 "n1481757
                                                            "n1481757
  selection-screen : begin of line.                         "n921165
    selection-screen position 33.
    parameters: archive  type mb5b_archive as checkbox default ' ' "n1481757
    user-command us_archive.                                "n1481757
    selection-screen comment 40(30) for field archive modif id sys.
  selection-screen end of line.
                                                            "n1481757
*  parameter for the archive info structure                     "n1481757
  parameters : pa_aistr    like aind_str1-archindex.        "n1481757
                                                            "n1481757
selection-screen end of block arch.                         "n1481757

* used for ABAP Unit Test see local class of CL_IM_RM07MLBD_DBSYS_OPT
parameters: p_aut type char1 no-display.

* -------------------- end of selection-sreen for archive---------------*

* ------------------- F4-Help --------- get info-structure -------------*
* datadefinition                                               "n1481757
data: g_f_f4_mode(01)  type  c,                             "n1481757
      g_f_f4_archindex like  aind_str1-archindex.           "n1481757
                                                            "n1481757

at selection-screen on value-request for p_vari.
  perform f4_for_variant.

*----------------------------------------------------------------------*

at selection-screen on value-request for pa_sflva.
  perform                    variant_value_request_f4
                             using  pa_sflva  g_s_vari_sumfl.

*----------------------------------------------------------------------*

at selection-screen on value-request for pa_suvar.
  perform                    variant_value_request_f4
                             using  pa_suvar g_s_vari_sumhq.

*-----------------------------------------------------------"n599218
* INITIALIZATION                                            "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218
* pay attentions : this process time will not be processed  "n599218
* in the webreporting mode                                  "n599218

initialization.
  "{ begin of MGV_GENERATED_RM07MLBD001                       "3203834->
  data: lt_sel_dtel type  rsseldtel occurs 0,
        ls_sel_dtel type  rsseldtel.

  ls_sel_dtel-name = 'MFRPN'.
  ls_sel_dtel-kind = 'S'.
  ls_sel_dtel-datenelment = 'MFRPN'.
  append ls_sel_dtel to lt_sel_dtel.

  call function 'SELECTION_TEXTS_MODIFY_DTEL'
    exporting
      program                     = sy-repid
    tables
      sel_dtel                    = lt_sel_dtel
    exceptions
      program_not_found           = 1
      program_cannot_be_generated = 2
      others                      = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  "{ end of MGV_GENERATED_RM07MLBD001                         "3203834<-

  clear : g_s_vari_sumhq, g_s_vari_sumfl.
  repid = sy-repid.
  variant_save = 'A'.

*  ENHANCEMENT-POINT rm07mlbd_03 SPOTS es_rm07mlbd.
* preprae the working areas for the variants
  move  : repid              to  g_s_vari_sumhq-report,
          'SUHQ'             to  g_s_vari_sumhq-handle,
          repid              to  g_s_vari_sumfl-report,
          'SUFL'             to  g_s_vari_sumfl-handle.

  move-corresponding : g_s_vari_sumhq  to  g_s_vari_sumhq_def,
                       g_s_vari_sumfl  to  g_s_vari_sumfl_def.

  perform  get_the_default_variant
                             using  pa_sflva
                                    g_s_vari_sumfl
                                    g_s_vari_sumfl_def.

  perform  get_the_default_variant
                             using  pa_suvar
                                    g_s_vari_sumhq
                                    g_s_vari_sumhq_def.

  perform initialisierung.

* get the parameters from the last run                      "n547170
  perform                    esdus_get_parameters.          "n547170

* set flag when INITILIZATION is processed
  move  'X'        to  g_flag_initialization.

* check switch FIN_LOCRU_SFWS_UI_02 activation
  gv_switch_ehp6ru = cl_fin_locru_switch_check=>fin_locru_sfws_ui_02( ).

* begin of secondary database settings                     "n1710850
  call function 'FUNCTION_EXISTS'
    exporting
      funcname           = c_hdb_dbcon_get
    exceptions
      function_not_exist = 1
      others             = 2.
  if sy-subrc = 0.
    call function c_hdb_dbcon_get
      exporting
        i_subappl        = c_hdb_subappl
        i_act_check_only = abap_true
      importing
        e_dbcon          = dbcon_active.
  endif.
* end of secondary database settings                       "n1710850
  perform check_ui_opti_badi.                               "1790231
  perform set_p_grid.                                       "UI Harmonization for S/4HC
* Check on general authorizations to display values and prices
  call method cl_mmim_auth=>perform_switched_auth_checks
    exporting
      i_scen_name = 'MM_IM_VALUES'
      i_static    = 'X'
    receiving
      e_allowed   = g_f_values_auth.
*-----------------------------------------------------------"n599218
* AT SELECTION-SCREEN                                       "n599218
*-----------------------------------------------------------"n599218

*----------- Prüfung der eingegebenen Selektionsparameter, ------------*
*---------------------- Berechtigungsprüfung --------------------------*

at selection-screen.

* the user will get the info about the old variant only     "n921165
* once                                                      "n921165
  if  g_cnt_error_dba = 1.                                  "n921165
    if  not sy-slset is initial.                            "n921165
*     Variant & of program & is not the current version     "n921165
      message i634(db)       with  sy-slset sy-repid.       "n921165
    endif.                                                  "n921165
  endif.                                                    "n921165
                                                            "n921165
* if the installed database system is not DB6, Informix or  "n921165
* MAxDB the parameter PA_DBSTD must be set                  "n921165
  if  g_flag_db_parameters is initial.                      "n921165
*  if the radio buttons are not shown,                      "n2308556
*  use standard access for all selections                   "n2308556
    move  'X'                to  pa_dbstd.                  "n2308556
    clear :                  pa_dbmat, pa_dbdat.            "n2308556
  endif.                                                    "n921165
                                                            "n921165
* check choosen database access agaist restrictions         "n921165
* text-095 : Mismatch Database access - restriction         "n921165
  if  g_flag_db_parameters = 'X'.                           "n921165
                                                            "n921165
    if      pa_dbmat = 'X'.                                 "n921165
*     access via material number : material entered ?       "n921165
      if  matnr[] is initial.                               "n921165
        set cursor         field  pa_dbmat.                 "n921165
        message  w895      with  text-115.                  "n921165
      endif.                                                "n921165
                                                            "n921165
    elseif  pa_dbdat = 'X'.                                 "n921165
*     access via posting data : posting date entered ?      "n921165
      if  datum-low  is initial and                         "n921165
          datum-high is initial.                            "n921165
        set cursor         field  pa_dbdat.                 "n921165
        message  w895      with  text-115.                  "n921165
      endif.                                                "n921165
                                                            "n921165
    endif.                                                  "n921165
  endif.                                                    "n921165

* the following 3 parameters XONUL, XVBST, and XNVBST       "n599218
* became obsolete; send error when they should be filled.   "n599218
* This could be possible if the user works with old         "n599218
* selection variants or this report is launched by a        "n599218
* SUBMIT command                                            "n599218
  if  xonul  is initial  and                                "n599218
      xvbst  is initial  and                                "n599218
      xnvbst is initial.                                    "n599218
*  ok, the old parameters are empty                         "n599218
  else.                                                     "n599218
*   text-088 : note 599218 : obsolete parameter used        "n599218
    message e895             with  text-088.                "n599218
  endif.

* did the user hit the pushbutton "Category" ?              "n599218
  case     sscrfields-ucomm.                                "n599218
    when  'LIU '.                                           "n599218
*     yes, the pushbutton "Category" was hit                "n599218
      if  g_flag_status_liu  =  c_hide.                     "n599218
*       show the 7 parameters on the selection srceen       "n599218
        move  c_show         to  g_flag_status_liu.         "n599218
      else.                                                 "n599218
*       hide the 7 paramaters                               "n599218
        move  c_hide         to  g_flag_status_liu.         "n599218
      endif.                                                "n599218
  endcase.                                                  "n599218

* carry out the "expensive" checks, like authorization,     "n878753
* only after the user wants to launch this report. In the   "n878753
* case an error message was sent the user can correct the   "n878753
* entries and go on with "ENTER". That means the system     "n878753
* field SY-UCOMM is initial. This correction should make    "n878753
* sure that all checks are done when this report is         "n878753
* launched.                                                 "n878753
  if  sy-ucomm = 'ONLI'  or                                 "n878753
      sy-ucomm = 'PRIN'  or                                 "n878753
      sy-ucomm = 'SJOB'.                                    "n878753
    move  'X'                to  g_flag_launched.           "n878753
  endif.                                                    "n878753
                                                            "n878753
  check : g_flag_launched = 'X'.                            "n878753

* only one sum function can be processed
  if  xsum     = 'X' and
      pa_sumfl = 'X'.
    set cursor               field 'XSUM'.
*   select one sum list only
    message  e895            with  text-093.
  endif.

  perform eingaben_pruefen.

  set cursor                 field 'PA_SFLVA'.
  perform  variant_check_existence
                             using     pa_sflva
                                       g_s_vari_sumfl
                                       g_s_vari_sumfl_def.

  set cursor                 field 'PA_SUVAR'.
  perform  variant_check_existence
                             using     pa_suvar
                                       g_s_vari_sumhq
                                       g_s_vari_sumhq_def.

* check whether FI summarization is active and other        "n547170
* restrictions could deliver wrong results                  "n547170
  perform                    f0800_check_restrictions.      "n547170

* - the user wants to surpress the reversal movements :     "n497992
*   process warning M7 392                                  "n497992
  if not nosto is initial.                                  "n497992
*   emerge warning ?                                        "n497992
    call function 'ME_CHECK_T160M'             "n497992
      exporting                                         "n497992
        i_arbgb = 'M7'                         "n497992
        i_msgnr = '392'                        "n497992
      exceptions                                        "n497992
        nothing = 0                            "n497992
        others  = 1.                           "n497992
                                                            "n497992
    if sy-subrc <> 0.                                       "n497992
      set cursor               field  'NOSTO'.              "n497992
*       to surpress the reversal movements could cause ...  "n497992
      message                  w392.                        "n497992
    endif.                                                  "n497992
  endif.                                                    "n497992

* carry out special authotity check for the tax auditor     "n547170
  perform                    tpc_check_tax_auditor.         "n547170

* does the user wants a selection via company code or a plant ?
* fill range table g_ra_werks
  refresh : g_ra_bwkey,  g_ra_werks, g_t_organ.
  clear   : g_ra_bwkey,  g_ra_werks, g_t_organ, g_s_organ.
  refresh : g_0000_ra_bwkey,  g_0000_ra_werks,  g_0000_ra_bukrs.
  clear   : g_0000_ra_bwkey,  g_0000_ra_werks,  g_0000_ra_bukrs.

  describe table  bukrs      lines  g_f_cnt_lines_bukrs.
  describe table  werks      lines  g_f_cnt_lines_werks.

  if  g_f_cnt_lines_bukrs  > 0  or
      g_f_cnt_lines_werks  > 0.
*   fill range tables for the CREATION OF TABLE G_T_ORGAN
    move : werks[]           to  g_0000_ra_werks[],
           bukrs[]           to  g_0000_ra_bukrs[].

    perform  f0000_create_table_g_t_organ
                             using  c_error.
  endif.

* ----- begin of note "n1481757 ---- check archive-info-structure----*
  data: g_flag_exist_as type c.                             "n1481757
  data : g_flag_too_many_sel(01) type c.                    "n1481757
  data: g_v_fieldname type fieldname.                       "n1481757
                                                            "n1481757
* process the MM docs from the new AS archive               "n1481757
  if archive = 'X'.                                         "n1481757
    perform check_existence_as using g_flag_exist_as.       "n1481757
    if  g_flag_exist_as = 'X'.                              "n1481757
      perform check_archive_index using g_flag_too_many_sel "n1481757
                                        g_v_fieldname.      "n1481757
      " Materialbelege aus dem Archiv auslesen              "n1481757
      if g_flag_too_many_sel = 'X'.                         "n1481757
        message w432  with  g_v_fieldname pa_aistr.         "n1481757
*     Eingrenzungen für Feld &1 wirken nicht                "n1481757
      endif.                                                "n1481757
    endif.                                                  "n1481757
  endif.                                                    "n1481757
* ----- end of note "n1481757 ----- check archive-info-structure ---*

* save the parameters of this run                           "n547170
  perform                    esdus_save_parameters.         "n547170

*-----------------------------------------------------------"n599218
* AT SELECTION-SCREEN OUTPUT                                "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218

at selection-screen output.                                 "n599218

* check whether the database access parameters fulfil the   "n921165
* radiobutton rules / in the case this report was launched  "n921165
* with a selection variant, the settings of this variant    "n921165
* have been set already                                     "n921165
  if  g_flag_db_parameters = 'X'.                           "n921165
    clear                    g_cnt_radio.                   "n921165
    if  pa_dbstd = 'X'.  add 1    to g_cnt_radio.  endif.   "n921165
    if  pa_dbmat = 'X'.  add 1    to g_cnt_radio.  endif.   "n921165
    if  pa_dbdat = 'X'.  add 1    to g_cnt_radio.  endif.   "n921165
                                                            "n921165
    if  g_cnt_radio = 1.                                    "n921165
*     ok                                                    "n921165
    else.                                                   "n921165
*     offended against radiobutton rules : set default      "n921165
      add  1                 to  g_cnt_error_dba.           "n921165
      move : 'X'             to  pa_dbstd.                  "n921165
      clear :                pa_dbmat, pa_dbdat.            "n921165
    endif.                                                  "n921165
  endif.                                                    "n921165

  if  g_flag_initialization is initial.                     "n599218
*   the process time INITIALIZATION was not done, so        "n599218
*   carry out the functions here                            "n599218
    move  'X'                to g_flag_initialization.      "n599218
                                                            "n599218
    perform                  initialisierung.               "n599218
                                                            "n599218
*   get the parameters from the last run                    "n599218
    perform                  esdus_get_parameters.          "n599218
  endif.                                                    "n599218
                                                            "n599218
* how to handle the 7 paramaters for the scope of list ?    "n599218
  loop at screen.
    case screen-name.
*   modify the selection screen                             "n599218
      when 'BWBST'.  "Valuted stock disabled/no auhtorization for values
        if g_f_values_auth is initial.
          screen-active = '0'.
        else.
          screen-active = '1'.
        endif.
        modify screen.
    endcase.                                                "n599218
    case    screen-group1.                                  "n599218
      when  'LIU'.                                          "n599218
        if  g_flag_status_liu  = c_show.                    "n599218
          screen-active = '1'.         "show parameters     "n599218
        else.                                               "n599218
          screen-active = '0'.         "Hide parameters     "n599218
        endif.                                              "n599218
                                                            "n599218
        modify screen.                                      "n599218

      when  'DBA'.                                          "n921165
*       show or hide the parametes for the database access  "n921165
        if  g_flag_db_parameters = 'X'.                     "n921165
          screen-active = '1'.         "show parameters     "n921165
        else.                                               "n921165
          screen-active = '0'.         "Hide parameters     "n921165
        endif.                                              "n921165
                                                            "n921165
        modify screen.                                      "n921165

      when  'HKT'.
*       show or hide HKONT parameter
        if gv_switch_ehp6ru = 'X'.
          screen-active = '1'.
        else.
          screen-active = '0'.
        endif.
        modify screen.

      when  'OPT'.                                          "1790231
        if gv_ui_opt_active = abap_false.                   "1790231
          screen-active = 0.                                "1790231
          modify screen.                                    "1790231
        endif.                                              "1790231


    endcase.                                                "n599218
  endloop.                                                  "n599218
                                                            "n599218
* adapt the icon on the pushbutton depending on the status  "n599218
  case    g_flag_status_liu.                                "n599218
    when  c_hide.                                           "n599218
      move  text-081         to  pb_liu.  "@0E\Q@ Scope ... "n599218
    when  c_show.                                           "n599218
      move  text-082         to  pb_liu.  "@0H\Q@ Scope ... "n599218
    when  others.                                           "n599218
  endcase.                                                  "n599218
                                                            "n599218
*-----------------------------------------------------------"n599218

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*

start-of-selection.

* NEW DB                                             "v hana_20120802
  data: gr_badi_rm07mlbd_dbsys_opt type ref to rm07mlbd_dbsys_opt,
        gv_newdb                   type abap_bool,
        gv_no_dbsys_opt            type abap_bool,
        gt_stock_inventory         type stock_inventory_tt,
        gs_stock_inventory         type stock_inventory_s.
  data: gv_unittest     type abap_bool,              "v hana_20120821
        bestand_opensql like table of bestand,
        bestand_new_db  like table of bestand.                "^ hana_20120821
  data: gv_optimization_active type abap_bool.              "n2122205
  data: lo_opti_badi           type ref to mm_perf_optimization. "n2122205
  field-symbols: <gs_stock_inventory> type stock_inventory_s.

  data: gv_dontpanic   type symsgv.                         "3116194

  if  ( pa_sumfl = abap_true or     "aggregate movements only
        xsum     = abap_true )
  and pa_wdzer = abap_true      "view full list scope only
  and pa_wdzew = abap_true
  and pa_wdwiz = abap_true
  and pa_wdwuw = abap_true
  and pa_wdwew = abap_true
  and pa_ndsto = abap_true
  and pa_ndzer = abap_true
  and sobkz    na 'OWV'
  and nosto    = abap_false     "no hiding of reversals
  and archive  = abap_false     "not with archived data
  and bwbst    = abap_false.    "no valuated stocks
    try.                                                    "v n2122205
        get badi lo_opti_badi.
      catch cx_badi_not_implemented
            cx_badi_multiply_implemented
            cx_badi_filter_error.
        gv_optimization_active  = abap_false.
    endtry.
    try.
        call badi lo_opti_badi->is_active
          exporting
            iv_reportname = sy-repid
          receiving
            rv_active     = gv_optimization_active.
      catch cx_badi.
        gv_optimization_active  = abap_false.
    endtry.
    if gv_optimization_active = abap_true.
      try.
          get badi gr_badi_rm07mlbd_dbsys_opt
            filters
              dbsys_type = cl_db_sys=>dbsys_type.
          gv_newdb = abap_true.
        catch cx_badi_not_implemented
              cx_badi_multiply_implemented
              cx_badi_filter_error.
          gv_newdb = abap_false.
      endtry.
    endif.                                                  "^ n2122205
  endif.                                             "^ hana_20120802
  if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP616_RM07MLBD_01\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* Optimization 616 - Stored procedures
*
*  p_aut    = parameter for ABAP UNIT tests (O = old version , N = with stored procedures)
*  gv_newdb = varaible to control and use the Stored procedures via BADI implementation
*             (stored procedures are for all DB existing, not only HANA)
    clear: p_aut.
* activate old DB logic and access (without stored procedures)
    gv_newdb = abap_false.
  endif.
*ENHANCEMENT-POINT ehp616_rm07mlbd_01 SPOTS es_rm07mlbd .

  if p_aut ne space.
* Code injection for ABAP UNIT TEST
* see local class of CL_IM_RM07MLBD_DBSYS_OPT
    case p_aut.
      when cl_mm_im_aut_master=>gc_aut_optimization_off.
* old version / without optimization
        gv_newdb = abap_false.
      when  cl_mm_im_aut_master=>gc_aut_optimization_on.
* "new version / with optimization
        gv_newdb = abap_true.
      when others.
        clear p_aut.
    endcase.
  endif.                       " w/o New DB feature  "^ hana_20120821

* Deactivate old MMIM optimization in SAPSCORE.
* because AMDP for HANA and ADBC for ANYDB are not redirected
* to the new datamodel, so there will be inconsistencies
* once db table NSDM_D_MTDCSA is active.
  gv_newdb = abap_false.  "FOR SAPSCORE

* check settings to activate new selection                    "v3116194
* a) check control table for system setting on new selection
  if gv_run_mode is initial.
    select count(*) from mmim_control_log where action = 'XOPVW'
                                          and   repid  = 'RM07MLBD'
                                          and   status = 'X'.
* The logic will be inverted: If there is no record in, then the ACDOCA selection is active.
* If this has to be deactivated the a record should be created.
* Program MB5B_TOGGLE_BSEG_ACDOCA in package MM_HOME can be used and provided as Z program to customers to togle the behavior.
    if sy-subrc is not initial.
      gv_run_mode = 'A'.
    endif.
  endif.
*b) check user setting
  get parameter id 'DONTPANIC' field  gv_dontpanic.
  if sy-subrc is initial.
    translate : gv_dontpanic to  upper case.             "#EC TRANSLANG
    find first occurrence of 'MB5BXOPVW'  in  gv_dontpanic.
    if  sy-subrc is initial.
      gv_run_mode = 'A'. "new selection active
    endif.
    find first occurrence of 'MB5BNOPVW'  in  gv_dontpanic.
    if  sy-subrc is initial.
      clear gv_run_mode. "set legacy selection active
    endif.
  endif.                                                    "^3116194

* it makes no sence to carry out this report with an old    "n921165
* and incorrect selection variant                           "n921165
  if  g_cnt_error_dba > 0.                                  "n921165
    if  not sy-slset is initial.                            "n921165
*     Variant & of program & is not the current version     "n921165
      message e634(db)       with  sy-slset sy-repid.       "n921165
    endif.                                                  "n921165
  endif.                                                    "n921165

* create the title line

* If no date is given at all, the range is set to the maximum
* extend (1.1.0000 - 31.12.9999).
* If only datum-low is set, it is interpreted as the day for
* which the analysis is wanted --> datum-high is filled up.
  if datum-low is initial.
    datum-low = '00000101'.
    if datum-high is initial.
      datum-high = '99991231'.
    endif.
  else.
    if datum-high is initial.
      datum-high = datum-low.
    endif.
  endif.
*  Begin of changes of note 1117067                        "n1117067
*  MOVE: datum-low(4)    TO jahrlow,
*        datum-low+4(2)  TO monatlow,
*        datum-low+6(2)  TO taglow,
*        datum-high(4)   TO jahrhigh,
*        datum-high+4(2) TO monathigh,
*        datum-high+6(2) TO taghigh.
*  SET TITLEBAR 'MAN'
*  WITH taglow monatlow jahrlow taghigh monathigh jahrhigh.
* Conversion of the dates from the internal to the external view
  call function 'CONVERT_DATE_TO_EXTERNAL'
    exporting
      date_internal            = datum-low
    importing
      date_external            = d_from
    exceptions
      date_internal_is_invalid = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  call function 'CONVERT_DATE_TO_EXTERNAL'
    exporting
      date_internal            = datum-high
    importing
      date_external            = d_to
    exceptions
      date_internal_is_invalid = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


  set titlebar 'MAN'
  with d_from d_to.
*  End of changes of note 1117067                          "n1117067


* create the headlines using the titelbar                   "n599218
  perform                    create_headline.               "n599218

* calculate the offsets for the list header
  perform                    calculate_offsets.

* for the representation of tied empties                    "n547170
  perform                    f0700_prepare_tied_empties.    "n547170

                                                            "n1784874
  break-point                id mmim_rep_mb5b.              "n921164
* dynamic break-point : is IS-OIL active ?                  "n921164
                                                            "n599218 A
* check whether this is a IS-OIL system                     "n599218 A
  perform                    check_is_oil_system.           "n599218 A
                                                            "n1784874

* create table g_t_mseg_fields with the names of all
* wanted fields from MSEG and MKPF
  perform                    f0300_get_fields.

* create the ALV fieldcatalog for the main list always
  move  'G_T_BELEGE'         to  g_f_tabname.

  perform                    f0400_create_fieldcat.

* do not print the ALV-statistics and selection criteria
  clear                      g_s_print.
  g_s_print-no_print_selinfos   = 'X'.
  g_s_print-no_print_listinfos = 'X'."

* create the range table for the storage location
  perform                    f0600_create_range_lgort.

* - show the current activity and the progress              "n599218
  call function 'SAPGUI_PROGRESS_INDICATOR'                 "n599218
    exporting                                               "n599218
      text = text-063.       "Reading current stocks        "n599218

  if gv_newdb = abap_true.                           "v hana_20120802
    perform new_db_run.
  endif.                                             "v hana_20120802

* get the stock tables
  perform                    aktuelle_bestaende.

  perform tabellen_lesen.

  if gv_newdb = abap_false. "~~~~~~~~~~~~~~~~~~~~~~ "hana_20120607_V1
* - show the current activity and the progress              "n599218
    call function 'SAPGUI_PROGRESS_INDICATOR'               "n599218
      exporting                                             "n599218
        text = text-064.       "Reading MM documents          "n599218
    perform                    f1000_select_mseg_mkpf.
  endif. "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ "hana_20120607_V1

  perform                    belegselektion.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*

end-of-selection.

* results of all the autority checks
  perform                    f9100_auth_plant_result.

  if gv_newdb = abap_false. "~~~~~~~~~~~~~~~~~~~~~~ "hana_20120607_V1
* - show the current activity and the progress              "n599218
    if bwbst = 'X'.                                         "n599218
      call function 'SAPGUI_PROGRESS_INDICATOR'               "n599218
        exporting                                             "n599218
          text = text-066.     "Calculating Stocks and Values "n599218
    else.                                                   "n599218
      call function 'SAPGUI_PROGRESS_INDICATOR'               "n599218
        exporting                                             "n599218
          text = text-067.     "Calculating Stocks            "n599218
    endif.                                                  "n599218

    perform summen_bilden.

    perform bestaende_berechnen.
  endif. "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ "hana_20120607_V1

  if p_aut ne space.
* Code injection for ABAP UNIT TEST
* see local class of CL_IM_RM07MLBD_DBSYS_OPT
*    EXPORT lt_bestand FROM bestand[]
    if gt_stock_inventory is initial.
      loop at bestand.
        move-corresponding bestand to gs_stock_inventory.
        insert gs_stock_inventory into table gt_stock_inventory.
      endloop.
    endif.
    export lt_bestand from gt_stock_inventory
      to memory id cl_mm_im_aut_master=>gc_memory_id_rm07mlbd.
    return.
  endif.

  perform listumfang.

* - show the current activity and the progress              "n599218
  call function 'SAPGUI_PROGRESS_INDICATOR'                 "n599218
    exporting                                               "n599218
      text = text-065.       "Preparing list output         "n599218

* stop if table bestand is empty
  describe table bestand     lines g_f_cnt_lines.

  if  g_f_cnt_lines is initial.
*   Keinen Eintrag zu den Suchbegriffen gefunden/selektiert
    message                  s083.
*   perform                  anforderungsbild.
  else.
*   process log function if the use is a tax auditor        "n555246
*   and the database selection was successful               "n555246
    if  g_flag_tpcuser = '1'.                               "n555246
      perform                tpc_write_log.                 "n555246
    endif.                                                  "n555246

    perform feldgruppen_aufbauen.

*   sort table with header data per material
    if bwbst is initial.
      sort bestand by matnr werks charg.
    else.
      sort bestand by matnr bwkey.
    endif.

*   which function does the user want ?
    if      xsum = 'X'.
*     hierseq. alv with sums
      perform                create_table_totals_hq.

      perform                create_fieldcat_totals_hq.

      perform                alv_hierseq_list_totals.

    elseif  pa_sumfl = 'X'.
*     show the sums only in a flat ALV
      perform                create_table_totals_flat.

      perform                create_fieldcat_totals_flat.

      perform                alv_flat_list_sums_only.

    else.
*     display the full list using the APPEND ALV
      perform                preenche_ksl.
      perform                bestaende_ausgeben.
    endif.
  endif.

  clear: g_t_mseg_lean, g_t_bsim_lean, bestand.             "n443935

*&---------------------------------------------------------------------*
*&   PF_STATUS_SET_TOTALS
*&---------------------------------------------------------------------*

form pf_status_set_totals                                   "#EC CALLED
                   using     extab type slis_t_extab.

  set pf-status 'STANDARD'   excluding extab.

endform.                     "PF_STATUS_SET_TOTALS

*----------------------------------------------------------------------*
*    user_parameters_save
*----------------------------------------------------------------------*

form user_parameters_save.

  get parameter id 'BUK'     field  g_save_params-bukrs.
  get parameter id 'WRK'     field  g_save_params-werks.
  get parameter id 'MAT'     field  g_save_params-matnr.
  get parameter id 'CHA'     field  g_save_params-charg.
  get parameter id 'BLN'     field  g_save_params-belnr.
  get parameter id 'BUK'     field  g_save_params-bukrs.
  get parameter id 'GJR'     field  g_save_params-gjahr.

endform.                     "user_parameters_save

*----------------------------------------------------------------------*
*    user_parameters_restore
*----------------------------------------------------------------------*

form user_parameters_restore.

  set parameter id 'BUK'     field  g_save_params-bukrs.
  set parameter id 'WRK'     field  g_save_params-werks.
  set parameter id 'MAT'     field  g_save_params-matnr.
  set parameter id 'CHA'     field  g_save_params-charg.
  get parameter id 'BLN'     field  g_save_params-belnr.
  get parameter id 'BUK'     field  g_save_params-bukrs.
  get parameter id 'GJR'     field  g_save_params-gjahr.

endform.                     "user_parameters_restore

*&---------------------------------------------------------------------*
*&   USER_COMMAND_TOTALS
*&---------------------------------------------------------------------*

form user_command_totals                                    "#EC CALLED
                   using     r_ucomm     like  sy-ucomm
                             rs_selfield type  slis_selfield.

  clear                      g_s_bestand_key.

  if      rs_selfield-tabname = 'G_T_TOTALS_HEADER'.
*   get the selected entry from table G_T_TOTALS
    read table g_t_totals_header
      into  g_s_totals_header
        index rs_selfield-tabindex.

    if sy-subrc is initial.
      move-corresponding  g_s_totals_header
                             to  g_s_bestand_key.
    endif.

  elseif  rs_selfield-tabname = 'G_T_TOTALS_ITEM'.
*   get the selected entry from table G_T_TOTALS
    read table g_t_totals_item
      into  g_s_totals_item
        index rs_selfield-tabindex.

    if sy-subrc is initial.
      move-corresponding  g_s_totals_item
                             to  g_s_bestand_key.
    endif.

  elseif  rs_selfield-tabname = 'G_T_TOTALS_FLAT'.
*   get the selected entry from table G_T_TOTALS
    read table g_t_totals_flat
      into  g_s_totals_flat
        index rs_selfield-tabindex.

    if sy-subrc is initial.
      move-corresponding  g_s_totals_flat
                             to  g_s_bestand_key.
    endif.
  endif.

  if g_s_bestand_key is initial.   "notinh found ?
*   Place the cursor on a table line
    message                  s322.
    exit.
  endif.

* get the line from the main table BESTAND depending on the mode
  if bwbst is initial.
*   sort sequence = matnr werks charg
    read table bestand
      with key  matnr = g_s_bestand_key-matnr
                werks = g_s_bestand_key-werks
                charg = g_s_bestand_key-charg
                             binary search.

  else.
*   sort sequence = matnr bwkey
    read table bestand
      with key  matnr = g_s_bestand_key-matnr
                bwkey = g_s_bestand_key-bwkey
                             binary search.
  endif.

  if sy-subrc is initial.
    move-corresponding bestand     to  g_s_bestand_detail.
    append  g_s_bestand_detail     to  g_t_bestand_detail.

    perform                  create_table_for_detail.

    perform                  list_output_detail.
  endif.

endform.                     " USER_COMMAND_TOTALS

*&---------------------------------------------------------------------*
* list_output_detail
*&---------------------------------------------------------------------*

form list_output_detail.

* build the auxiliary interface tables for the ALV

  if  g_cust_color = 'X'.              "colorize numeric fields ?
    layout-coltab_fieldname = 'FARBE_PRO_FELD'.
  else.
    layout-info_fieldname   = 'FARBE_PRO_ZEILE'.
  endif.

  layout-f2code = '9PBP'.

  if not bwbst is initial.
    layout-min_linesize = '92'.
  endif.

  events-name = 'TOP_OF_PAGE'.
  events-form = 'UEBERSCHRIFT_DETAIL'.
  append events.

  if  g_flag_break-b3 = 'X'.                                "n921164
    break-point              id mmim_rep_mb5b.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  endif.                                                    "n921164

  if gv_ui_opt_active = abap_false or p_grid = abap_false   "1790231
     or pa_sumfl = space.                                   "1790231
    call function 'REUSE_ALV_LIST_DISPLAY'
      exporting
        i_interface_check        = g_flag_i_check             "n599218
        i_callback_program       = repid
        i_callback_pf_status_set = 'STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        is_layout                = layout
        it_fieldcat              = fieldcat[]
        it_special_groups        = gruppen[]
        it_sort                  = sorttab[]
        i_default                = 'X'
        i_save                   = 'A'
        is_variant               = variante
        it_events                = events[]
        is_print                 = g_s_print
      tables
        t_outtab                 = g_t_belege1
      exceptions
        others                   = 2.

  else.                                                     "1790231
                                                            "1790231
    call function 'REUSE_ALV_GRID_DISPLAY'                  "1790231
      exporting                                             "1790231
        i_interface_check        = g_flag_i_check
        i_callback_program       = repid
        i_callback_pf_status_set = 'STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        is_layout                = layout
        it_fieldcat              = fieldcat[]
        it_special_groups        = gruppen[]
        it_sort                  = sorttab[]
        i_default                = 'X'
        i_save                   = 'A'
        is_variant               = variante
        it_events                = events[]
        is_print                 = g_s_print
      tables
        t_outtab                 = g_t_belege1
      exceptions
        others                   = 2.                       "1790231
                                                            "1790231
  endif.                                                    "1790231

* does the ALV return with an error ?
  if  not sy-subrc is initial.         "Fehler vom ALV ?
    message id sy-msgid type  'S'     number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                     " list_output_detail

*&---------------------------------------------------------------------*
*&   TOP_OF_PAGE_TOTALS
*&---------------------------------------------------------------------*

form top_of_page_totals.                                    "#EC CALLED

* go on when the report runs in print mode -> last line
  check not sy-prdsn is initial.

  data: lr_content type ref to cl_salv_form_element.

*... (1) create the information to be displayed by using
*        the ALV Form elements
  perform top_of_page_totals_render  changing lr_content.

*... (2) Sending the information to the ALV
*        Once the inforation to be displayed has been
*        created the information has to be sent to the ALV
*        This is done by calling the static method
*        CL_SALV_FORM_CONTENT=>SET( <content> ) with the content
*        which is to be displayed.
*        Alternativly the function module REUSE_ALV_COMMENTARY_WRITE
*        can still be used.
  cl_salv_form_content=>set( lr_content ).

endform.                     "TOP_OF_PAGE_TOTALS

*&---------------------------------------------------------------------*
*&   TOP_OF_PAGE_TOTALS_RENDER
*&---------------------------------------------------------------------*

form top_of_page_totals_render
         changing cr_content type ref to cl_salv_form_element.

  data: lr_grid     type ref to cl_salv_form_layout_grid,
        lr_flow     type ref to cl_salv_form_layout_flow,
        l_text(500) type c,
        l_char(500) type c.

*... create a grid
  create object lr_grid.

  lr_flow = lr_grid->create_flow( row = 1  column = 1 ).

  if  bwbst is initial.                                     "n599218
*   stocks only                                             "n599218
    write : sy-pagno no-sign      to  g_s_header_77-page.   "n599218
    move  : g_s_header_77         to  l_text.
  else.                                                     "n599218
*   stocks and values                                       "n599218
    write : sy-pagno no-sign      to  g_s_header_91-page.   "n599218
    move  : g_s_header_91         to  l_text.               "n599218
  endif.                                                    "n599218

* add line to object
  lr_flow->create_text( text = l_text ).

* copy whole header object
  cr_content = lr_grid.

endform.                     " TOP_OF_PAGE_TOTALS_RENDER

*----------------------------------------------------------------------*
* top_of_page_render.
*----------------------------------------------------------------------*

form top_of_page_render.

* interface structurebegin of g_s_bestand.

  data: lr_content type ref to cl_salv_form_element.

*... (1) create the information to be displayed by using
*        the ALV Form elements
  perform create_alv_form_content_top changing lr_content.

*... (2) Sending the information to the ALV
*        Once the inforation to be displayed has been
*        created the information has to be sent to the ALV
*        This is done by calling the static method
*        CL_SALV_FORM_CONTENT=>SET( <content> ) with the content
*        which is to be displayed.
*        Alternativly the function module REUSE_ALV_COMMENTARY_WRITE
*        can still be used.
  cl_salv_form_content=>set( lr_content ).

endform.                     " top_of_page_render

*----------------------------------------------------------------------*
* create_alv_form_content_top
*----------------------------------------------------------------------*
* baustelle

form create_alv_form_content_top
                   changing cr_content type ref to cl_salv_form_element.

  data: lr_grid     type ref to cl_salv_form_layout_grid,
        lr_flow     type ref to cl_salv_form_layout_flow,
        l_text(500) type c,
        l_char(500) type c.

  data: l_row                   type i,
        l_figure(24)            type c,
        l_flag_tied_empties(01) type c.

  data: l_f_text(60)        type  c.                        "n999530

*----------------------------------------------------------------------*


*... create a grid
  create object lr_grid.

* the current data are in interface structure g_s_bestand.

* in the case the report run in print or background mode
* --> print the old headlines

  if not sy-prdsn is initial.
    add  1                    to  l_row.
    lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

    if  bwbst is initial.
*     stocks only
      write : sy-pagno no-sign    to  g_s_header_77-page.
      move  : g_s_header_77       to  l_text.
    else.
*     stocks and values
      write : sy-pagno no-sign    to  g_s_header_91-page.
      move  : g_s_header_91       to  l_text.
    endif.

*   add line to object
    lr_flow->create_text( text = l_text ).

    add  1                    to  l_row.
  endif.

* first line : plant or valuation area ---------------------------------
  add  1                    to  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  if  bwbst is initial.
    perform  f2200_read_t001 using g_s_bestand-werks.       "n999530

    write g_s_bestand-werks  to l_f_text.                   "n999530
    condense l_f_text.                                      "n999530
    concatenate l_f_text     t001w-name1                    "n999530
                             into  l_f_text                 "n999530
                             separated by space.            "n999530

    move : text-020          to  l_text,
           l_f_text          to  l_text+g_offset_header.    "n999530
  else.
*   show valuation area
    move : text-025          to  l_text,
           g_s_bestand-bwkey to  l_text+g_offset_header.
  endif.

* add line to object
  lr_flow->create_text( text = l_text ).

* second line : material number ----------------------------------------
  add   1                    to  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  move  : text-021           to  l_text.
  write : g_s_bestand-matnr  to  l_text+g_offset_header.

* add line to object
  lr_flow->create_text( text = l_text ).

* third line : material short text -------------------------------------
  add   1                    to  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  perform  f2100_mat_text    using  g_s_bestand-matnr.

  move : text-022            to  l_text,
         g_s_makt-maktx      to  l_text+g_offset_header.

* add line to object
  lr_flow->create_text( text = l_text ).

* fourth line : batch if required --------------------------------------
  if xchar = 'X'.
    add   1                  to  l_row.
    lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

    move : text-023           to  l_text,
           g_s_bestand-charg  to  l_text+g_offset_header.

*   add line to object
    lr_flow->create_text( text = l_text ).
  endif.

* line : stock and value on start date ------------------------------
* with one empty line
  add  2                     to  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

* convert unit of measurement from internal to external format "n1018717
  write : g_s_bestand-meins       to  l_f_meins_external.   "n1018717

  clear                           l_text.
  if bwbst is initial.
*   stock on start date
    move : g_date_line_from       to  l_text.
    write  g_s_bestand-anfmenge   to l_figure
                                  unit  g_s_bestand-meins.
    move  l_figure                to  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    move  l_f_meins_external     to  l_text+g_offset_unit.  "n1018717
    if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_01\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* Stock on start date in PUM
      write g_s_bestand-/cwm/anfmenge to l_figure
                                      unit g_s_bestand-/cwm/meins.
      move  l_figure                  to  l_text+gv_/cwm/offset_qty(24).
      write g_s_bestand-/cwm/meins    to  l_text+gv_/cwm/offset_unit.
    endif.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_01 SPOTS es_rm07mlbd .
  else.
* stocks and values on start date
    move : g_date_line_from       to  l_text.
    write  g_s_bestand-anfmenge   to l_figure
                                  unit  g_s_bestand-meins.
    move  l_figure                to  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    move  l_f_meins_external     to  l_text+g_offset_unit.  "n1018717


    write g_s_bestand-anfwert     to l_figure
                                  currency  g_s_bestand-waers.
    move  l_figure                to  l_text+g_offset_value(24).
    move  g_s_bestand-waers       to  l_text+g_offset_curr.
  endif.

*   add line to object
  lr_flow->create_text( text = l_text ).

* line : total quantity and value of goods receipts --------------------
  add  1                     to  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  clear                           l_text.

  if bwbst is initial.
*   total quantities of goods receipts
    move : text-005               to  l_text+2.
    write  g_s_bestand-soll       to l_figure
                                  unit  g_s_bestand-meins.
    move  l_figure                to  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    move  l_f_meins_external      to  l_text+g_offset_unit. "n1018717
    if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_02\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* total quantities of goods receipts in PUM
      write g_s_bestand-/cwm/soll     to l_figure
                                      unit g_s_bestand-/cwm/meins.
      move  l_figure                  to  l_text+gv_/cwm/offset_qty(24).
      write g_s_bestand-/cwm/meins    to  l_text+gv_/cwm/offset_unit.
    endif.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_02 SPOTS es_rm07mlbd .
  else.
*   total quantities and values of goods receipts
    move : text-030               to  l_text+2.
    write  g_s_bestand-soll       to l_figure
                                  unit  g_s_bestand-meins.
    move  l_figure                to  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    move  l_f_meins_external      to  l_text+g_offset_unit. "n1018717

*    ENHANCEMENT-POINT ehp605_rm07mlbd_03 SPOTS es_rm07mlbd .
    write g_s_bestand-sollwert     to l_figure
                                  currency  g_s_bestand-waers.
    move  l_figure                to  l_text+g_offset_value(24).
    move  g_s_bestand-waers       to  l_text+g_offset_curr.
  endif.

* add line to object
  lr_flow->create_text( text = l_text ).

* line : total quantity and value of goods issues ----------------------
  add  1                     to  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  clear                           l_text.

  if bwbst is initial.
*   total quantities of goods issues
    move : text-006               to  l_text+2.
    compute  g_s_bestand-haben    =  g_s_bestand-haben * -1.
    write  g_s_bestand-haben      to l_figure
                                  unit  g_s_bestand-meins.
    move  l_figure                to  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    move  l_f_meins_external      to  l_text+g_offset_unit. "n1018717
    if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_04\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* total quantities of goods issues
      compute g_s_bestand-/cwm/haben =  g_s_bestand-/cwm/haben * -1.
      write g_s_bestand-/cwm/haben   to l_figure
                                     unit g_s_bestand-/cwm/meins.
      move  l_figure                 to  l_text+gv_/cwm/offset_qty(24).
      write g_s_bestand-/cwm/meins   to  l_text+gv_/cwm/offset_unit.
    endif.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_04 SPOTS es_rm07mlbd .
  else.
*   total quantities of goods issues
    move : text-031               to  l_text+2.
    compute  g_s_bestand-haben    =  g_s_bestand-haben * -1.
    write  g_s_bestand-haben      to l_figure
                                  unit  g_s_bestand-meins.
    move  l_figure                to  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    move  l_f_meins_external      to  l_text+g_offset_unit. "n1018717
    if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_05\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* total quantities of goods issues
      compute g_s_bestand-/cwm/haben =  g_s_bestand-/cwm/haben * -1.
      write g_s_bestand-/cwm/haben   to l_figure
                                     unit g_s_bestand-/cwm/meins.
      move  l_figure                 to  l_text+gv_/cwm/offset_qty(24).
      write g_s_bestand-/cwm/meins   to  l_text+gv_/cwm/offset_unit.
    endif.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_05 SPOTS es_rm07mlbd .
    compute g_s_bestand-habenwert  =  g_s_bestand-habenwert * -1.
    write g_s_bestand-habenwert   to l_figure
                                  currency  g_s_bestand-waers.
    move  l_figure                to  l_text+g_offset_value(24).
    move  g_s_bestand-waers       to  l_text+g_offset_curr.
  endif.

* add line to object
  lr_flow->create_text( text = l_text ).

* line : stock and value on end date ------------------------------
  add  1                     to  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  clear                           l_text.

  if bwbst is initial.
*   stock on end date
    move : g_date_line_to         to  l_text.
    write  g_s_bestand-endmenge   to l_figure
                                  unit  g_s_bestand-meins.
    move  l_figure                to  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    move  l_f_meins_external      to  l_text+g_offset_unit. "n1018717
*    ENHANCEMENT-POINT ehp605_rm07mlbd_06 SPOTS es_rm07mlbd .
  else.
* stocks and values on end date
    move : g_date_line_to         to  l_text.
    write  g_s_bestand-endmenge   to l_figure
                                  unit  g_s_bestand-meins.
    move  l_figure                to  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    move  l_f_meins_external      to  l_text+g_offset_unit. "n1018717
    if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_07\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* Stock on end date
      write g_s_bestand-/cwm/endmenge to l_figure
                                      unit g_s_bestand-/cwm/meins.
      move  l_figure                to  l_text+gv_/cwm/offset_qty(24).
      write g_s_bestand-/cwm/meins  to  l_text+gv_/cwm/offset_unit.
    endif.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_07 SPOTS es_rm07mlbd .
    write g_s_bestand-endwert     to l_figure
                                  currency  g_s_bestand-waers.
    move  l_figure                to  l_text+g_offset_value(24).
    move  g_s_bestand-waers       to  l_text+g_offset_curr.
  endif.

* add line to object
  lr_flow->create_text( text = l_text ).

* copy whole header object
  cr_content = lr_grid.

endform.                    " create_alv_form_content_top

*----------------------------------------------------------------------*
*    create_table_totals_hq
*----------------------------------------------------------------------*

form create_table_totals_hq.

* create output table
  loop at bestand.
*   part 1 : create header table g_t_totals_header
    move-corresponding  bestand        to  g_s_totals_header.
    move  sobkz                        to  g_s_totals_header-sobkz.

    perform  f2100_mat_text  using  bestand-matnr.
    move  g_s_makt-maktx     to  g_s_totals_header-maktx.

    if  bwbst is initial.
*     mode : stocks or special stocks
      perform  f2200_read_t001 using bestand-werks.

      move  t001w-name1      to  g_s_totals_header-name1.
    else.
*     mode : valuated stocks
      if  curm = '3'.
*       valuation level is company code
        select single butxt  from t001
                             into g_f_butxt
        where  bukrs = bestand-bwkey.

        if sy-subrc is initial.
          move  g_f_butxt    to  g_s_totals_header-name1.
        else.
          clear              g_s_totals_header-name1.
        endif.
      else.
*       valuation level is plant -> take the name of the plant
        perform  f2200_read_t001 using bestand-werks.

        move  t001w-name1    to  g_s_totals_header-name1.
      endif.
    endif.

    append  g_s_totals_header     to  g_t_totals_header.

*   part 2 : create 4 lines in item table g_t_totals_item
    clear                         g_s_totals_item.
    move : bestand-bwkey          to  g_s_totals_item-bwkey,
           bestand-werks          to  g_s_totals_item-werks,
           bestand-matnr          to  g_s_totals_item-matnr,
           bestand-charg          to  g_s_totals_item-charg,
           bestand-meins          to  g_s_totals_item-meins,
           bestand-waers          to  g_s_totals_item-waers.

*   line with the stock on start date
    move : g_date_line_from       to  g_s_totals_item-stock_type,
           bestand-anfmenge       to  g_s_totals_item-menge,
           bestand-anfwert        to  g_s_totals_item-wert.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_08\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
    move : bestand-/cwm/anfmenge  to  g_s_totals_item-/cwm/menge,
           bestand-/cwm/meins     to  g_s_totals_item-/cwm/meins.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_08 SPOTS es_rm07mlbd .
    perform                       create_table_totals_hq_1.

*   line with the good receipts
    if  bwbst = 'X'.
      move : text-030             to  g_s_totals_item-stock_type+2,
             bestand-soll         to  g_s_totals_item-menge,
             bestand-sollwert     to  g_s_totals_item-wert.
    else.
      move : text-005             to  g_s_totals_item-stock_type+2,
             bestand-soll         to  g_s_totals_item-menge.
    endif.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_09\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* line with teh goods receipts
    move : bestand-/cwm/soll    to  g_s_totals_item-/cwm/menge,
           bestand-/cwm/meins   to  g_s_totals_item-/cwm/meins.

*    ENHANCEMENT-POINT ehp605_rm07mlbd_09 SPOTS es_rm07mlbd .
    perform                       create_table_totals_hq_1.

*   line with the good issues
    if  bwbst = 'X'.
      move : text-031             to  g_s_totals_item-stock_type+2.
      g_s_totals_item-menge       = bestand-haben      * -1.
      g_s_totals_item-wert        = bestand-habenwert  * -1.
    else.
      move : text-006             to  g_s_totals_item-stock_type+2.
      g_s_totals_item-menge       = bestand-haben      * -1.
    endif.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_10\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* line with the good issues
    g_s_totals_item-/cwm/menge  = bestand-/cwm/haben * -1.
    move : bestand-/cwm/meins     to  g_s_totals_item-/cwm/meins.

*    ENHANCEMENT-POINT ehp605_rm07mlbd_10 SPOTS es_rm07mlbd .
    perform                       create_table_totals_hq_1.

*   line with the tock on end date
    move : g_date_line_to         to  g_s_totals_item-stock_type,
           bestand-endmenge       to  g_s_totals_item-menge,
           bestand-endwert        to  g_s_totals_item-wert.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_11\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* line with the stock on end date
    move : bestand-/cwm/endmenge  to  g_s_totals_item-/cwm/menge,
           bestand-/cwm/meins     to  g_s_totals_item-/cwm/meins.

*    ENHANCEMENT-POINT ehp605_rm07mlbd_11 SPOTS es_rm07mlbd .
    perform                       create_table_totals_hq_1.
  endloop.

endform.                     " create_table_totals_hq

*----------------------------------------------------------------------*
* create_table_totals_hq_1.
*----------------------------------------------------------------------*

* colorize the numeric fields depending on the sign and append the
* entries into table G_T_TOTALS_ITEM

form create_table_totals_hq_1.

  refresh                    g_t_color.
  clear                      g_s_color.

* colorize the quntities always
  if      g_s_totals_item-menge > 0.
*   positive value -> green
    move : 'MENGE'           to  g_s_color-fieldname,
           '5'               to  g_s_color-color-col,    "green
           '0'               to  g_s_color-color-int.
    append  g_s_color        to  g_t_color.

    move : 'MEINS'           to  g_s_color-fieldname,
           '5'               to  g_s_color-color-col,    "green
           '0'               to  g_s_color-color-int.
    append  g_s_color        to  g_t_color.

  elseif  g_s_totals_item-menge < 0.
*   negative value -> red
    move : 'MENGE'           to  g_s_color-fieldname,
           '6'               to  g_s_color-color-col,    "red
           '0'               to  g_s_color-color-int.
    append  g_s_color        to  g_t_color.

    move : 'MEINS'           to  g_s_color-fieldname,
           '6'               to  g_s_color-color-col,    "red
           '0'               to  g_s_color-color-int.
    append  g_s_color        to  g_t_color.
  endif.

  if  bwbst = 'X'.
*  colorize the values only in mode valuated stock
    if      g_s_totals_item-wert > 0.
*     positive value -> green
      move : 'WERT'          to  g_s_color-fieldname,
             '5'             to  g_s_color-color-col,    "green
             '0'             to  g_s_color-color-int.
      append  g_s_color      to  g_t_color.

      move : 'WAERS'         to  g_s_color-fieldname,
             '5'             to  g_s_color-color-col,    "green
             '0'             to  g_s_color-color-int.
      append  g_s_color      to  g_t_color.

    elseif  g_s_totals_item-wert < 0.
*     negative value -> red
      move : 'WERT'          to  g_s_color-fieldname,
             '6'             to  g_s_color-color-col,    "red
             '0'             to  g_s_color-color-int.
      append  g_s_color      to  g_t_color.

      move : 'WAERS'         to  g_s_color-fieldname,
             '6'             to  g_s_color-color-col,    "red
             '0'             to  g_s_color-color-int.
      append  g_s_color      to  g_t_color.
    endif.
  endif.

  if  g_t_color[] is initial.
    clear :                  g_s_totals_item-color.
  else.
*   customizing : set the color information
    if  g_cust_color  = 'X'.
      move  g_t_color[]      to  g_s_totals_item-color.
    endif.
  endif.

  add   1                    to  g_s_totals_item-counter.
  append  g_s_totals_item    to  g_t_totals_item.
  clear :                    g_s_totals_item-stock_type.

endform.                     " create_table_totals_hq_1.

*----------------------------------------------------------------------*
*    create_table_for_detail
*----------------------------------------------------------------------*

form create_table_for_detail.

  statics : l_flag_sorted(01)     type  c.
  data    : l_tabix  like  sy-tabix,
            vdocln   like  faglflexa-docln,
            vksl     like  faglflexa-ksl,
            vg_dmbtr like faglflexa-ksl.

  if gv_newdb = abap_true.
*   read it134m from db in form kontiert_aussortieren      "1784986v2!
    refresh g_t_mseg_lean.
    refresh matnr.
    matnr-sign = 'I'.
    matnr-option = 'EQ'.
    matnr-low = g_s_bestand_detail-matnr.
    matnr-high = space.
    append matnr.
    perform f1000_select_mseg_mkpf.
    perform belege_sortieren.
    perform summen_bilden.                                  "1784986
    select matnr meins mtart from mara                      "1784986
      into corresponding fields of table imara              "1784986
      where  matnr  =  g_s_bestand_detail-matnr             "1784986
    order by primary key.                                   "1858578
    perform kontiert_aussortieren.                          "1784986
    clear l_flag_sorted.
  endif.

* sort table with the documents
  if  l_flag_sorted is initial.
    sort  g_t_mseg_lean
      by matnr werks charg budat mblnr zeile belnr.
    move  'X'                to  l_flag_sorted.
  endif.

  refresh                    g_t_belege1.

* find the first entry with this material number
  read table g_t_mseg_lean   into  g_s_mseg_lean
    with key matnr = g_s_bestand_detail-matnr
      binary search.

  if  sy-subrc is initial.
    move  sy-tabix           to  l_tabix.

    loop at g_t_mseg_lean   into  g_s_mseg_lean
                             from l_tabix.

*     leave this loop when the material number changes
      if  g_s_mseg_lean-matnr  ne  g_s_bestand_detail-matnr.
        exit.
      endif.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = g_s_mseg_lean-buzei
        importing
          output = vdocln.

      if vdocln is initial or vdocln = '000000'.
        if g_s_mseg_lean-shkzg = 'H'.
          vg_dmbtr = g_s_mseg_lean-dmbtr * -1.
        else.
          vg_dmbtr = g_s_mseg_lean-dmbtr.
        endif.
        select single ksl
          from faglflexa
          into vksl
          where belnr = g_s_mseg_lean-belnr
          and   rbukrs = g_s_mseg_lean-bukrs
          and   rldnr = '0L'
          and   hsl   = vg_dmbtr.
      else.
        select single ksl
          from faglflexa
          into vksl
          where belnr = g_s_mseg_lean-belnr
          and   docln = vdocln
          and   rbukrs = g_s_mseg_lean-bukrs
          and   rldnr = '0L'.
      endif.
      if sy-subrc ne 0.
        clear vksl.
      endif.

      if  bwbst is initial.
        check : g_s_mseg_lean-werks = bestand-werks.        "n1390970
        check : xchar               is initial       or
                g_s_mseg_lean-charg = bestand-charg.
        move-corresponding g_s_mseg_lean
                             to  g_t_belege1.

        move vksl to g_t_belege1-ksl.

*       enrich some fields with color and numeric fields with sign
        perform  f9500_set_color_and_sign
                       using  g_t_belege1  'G_T_BELEGE1'.
        append                g_t_belege1.
      else.
*       get the valuation area for this plant
        perform  f9300_read_organ
                   using     c_werks   g_s_mseg_lean-werks.

        check : g_s_organ-bwkey = bestand-bwkey.            "184465
        move-corresponding  g_s_mseg_lean
                             to  g_t_belege1.

*       enrich some fields with color and numeric fields with sign
        perform  f9500_set_color_and_sign
                       using  g_t_belege1  'G_T_BELEGE1'.

        append               g_t_belege1.
      endif.
    endloop.
  endif.

endform.                     " create_table_for_detail

*----------------------------------------------------------------------*
* create_table_totals_flat
*----------------------------------------------------------------------*

form create_table_totals_flat.
  data: l_lgobe     type t001l-lgobe.

*--CS2020001127 - Jaime Tassoni - 04.11.2020 - inicio
  if p_xlgort is initial.
    free: x_bestand.
    loop at bestand.
      clear bestand-lgort.
      if sy-tabix > 1.
        clear: bestand-soll,
               bestand-haben.
      endif.
      move bestand  to x_bestand.
      collect x_bestand.
    endloop.
    bestand[] = x_bestand[].
  endif.
*--CS2020001127 - Jaime Tassoni - 04.11.2020 - fim
* create output table G-T-totals_flat
  loop at bestand.
    refresh                  g_t_color.
    move-corresponding  bestand        to  g_s_totals_flat.
    move  sobkz                        to  g_s_totals_flat-sobkz.

    perform  f2100_mat_text  using  bestand-matnr.

*   show the GI with negative sign
    g_s_totals_flat-haben         = g_s_totals_flat-haben     * -1.

    move : g_s_makt-maktx    to  g_s_totals_flat-maktx,
           datum-low         to  g_s_totals_flat-start_date,
           datum-high        to  g_s_totals_flat-end_date.

    perform  colorize_totals_flat   using 'ANFMENGE'.
    perform  colorize_totals_flat   using 'SOLL'.
    perform  colorize_totals_flat   using 'HABEN'.
    perform  colorize_totals_flat   using 'ENDMENGE'.
    if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_12\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
*
      g_s_totals_flat-/cwm/haben    = g_s_totals_flat-/cwm/haben * -1.
      perform  colorize_totals_flat   using '/CWM/ANFMENGE'.
      perform  colorize_totals_flat   using '/CWM/SOLL'.
      perform  colorize_totals_flat   using '/CWM/HABEN'.
      perform  colorize_totals_flat   using '/CWM/ENDMENGE'.

    endif.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_12 SPOTS es_rm07mlbd .

    if  bwbst = 'X'.
      g_s_totals_flat-habenwert     = g_s_totals_flat-habenwert * -1.

      perform  colorize_totals_flat   using 'ANFWERT'.
      perform  colorize_totals_flat   using 'SOLLWERT'.
      perform  colorize_totals_flat   using 'HABENWERT'.
      perform  colorize_totals_flat   using 'ENDWERT'.
    endif.

    if  g_t_color[] is initial.
      clear                  g_s_totals_flat-color.
    else.
      move  g_t_color[]      to  g_s_totals_flat-color.
    endif.

* get the name of this plant                                "n999530
    perform f2200_read_t001  using  g_s_totals_flat-werks.  "n999530

    move  t001w-name1        to  g_s_totals_flat-name1.     "n999530

*--CS2020001127 - Jaime Tassoni - 04.11.2020 - inicio
    clear l_lgobe.
    select lgobe
      into l_lgobe
      from t001l
        up to 1 rows
     where werks = bestand-werks
       and lgort = bestand-lgort.
    endselect.
    move l_lgobe             to g_s_totals_flat-lgobe.
*--CS2020001127 - Jaime Tassoni - 04.11.2020 - fim

    append  g_s_totals_flat  to  g_t_totals_flat.
  endloop.

endform.                     " create_table_totals_flat

*----------------------------------------------------------------------*
*   colorize_totals_flat
*----------------------------------------------------------------------*

form  colorize_totals_flat   using l_fieldname type any.

  data : l_f_fieldname(30)   type c.
  field-symbols : <l_fs_field>.

* customizing : set the color information
  check : g_cust_color  = 'X'.

  concatenate  'G_S_TOTALS_FLAT-' l_fieldname
                             into l_f_fieldname.
  assign (l_f_fieldname)     to  <l_fs_field>.

  check sy-subrc is initial.

  if      <l_fs_field> > 0.
    move : l_fieldname       to  g_s_color-fieldname,
           '5'               to  g_s_color-color-col,    "green
           '0'               to  g_s_color-color-int.
    append  g_s_color        to  g_t_color.

  elseif  <l_fs_field> < 0.
    move : l_fieldname       to  g_s_color-fieldname,
         '6'               to  g_s_color-color-col,    "red
         '0'               to  g_s_color-color-int.
    append  g_s_color        to  g_t_color.

  endif.

endform.                     " colorize_totals_flat

*----------------------------------------------------------------------*
* create_fieldcat_totals_flat
*----------------------------------------------------------------------*

form create_fieldcat_totals_flat.

  clear : g_s_fieldcat,      g_f_col_pos.

  if  bwbst = 'X'.
*   valuated stock
    perform fc_s_flat using 'BWKEY' 'MBEW' 'BWKEY'.
  else.
*   take the plant
    perform fc_s_flat using 'WERKS' 'MARC' 'WERKS'.

    if  xchar = 'X'.
*     take the batch number
      perform fc_s_flat using 'CHARG' 'MCHB' 'CHARG'.
    endif.
  endif.

  if bwbst is initial.                                      "n999530
    move  'X'                 to  g_s_fieldcat-no_out.      "n999530
    move : text-024           to  g_s_fieldcat-seltext_l,   "n999530
          'L'                 to  g_s_fieldcat-ddictxt,     "n999530
          30                  to  g_s_fieldcat-outputlen.   "n999530
    perform fc_s_flat using 'NAME1' 'T001W' 'NAME1'.        "n999530
  endif.                                                    "n999530

  perform fc_s_flat using 'MATNR' 'MARA' 'MATNR'.

  move  'X'                  to  g_s_fieldcat-no_out.
  perform fc_s_flat using 'MAKTX' 'MAKT' 'MAKTX'.

  if  sobkz is initial.
    move  'X'                to  g_s_fieldcat-no_out.
  endif.

  perform fc_s_flat using 'SOBKZ' 'MSLB' 'SOBKZ'.

**********************************************************************
**** START OF NOTE 1064332                                   "n1064332
**** new logic for fields start_date & end_date              "n1064332
**********************************************************************

  add  : 1                   to  g_f_col_pos.
  move : 'START_DATE'        to  g_s_fieldcat-fieldname,
         g_f_col_pos         to  g_s_fieldcat-col_pos,
         'G_T_TOTALS_FLAT'   to  g_s_fieldcat-tabname,
         text-094            to  g_s_fieldcat-seltext_l, "from date
         text-094            to  g_s_fieldcat-seltext_m, "from date
         text-094            to  g_s_fieldcat-seltext_s, "from date
         'L'                 to  g_s_fieldcat-ddictxt,
         15                  to  g_s_fieldcat-outputlen,
         'D'                 to  g_s_fieldcat-inttype,
         'DATS'              to  g_s_fieldcat-datatype.
* fields l_ref_* are no longer needed
*         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
*         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.

  append  g_s_fieldcat       to  g_t_fieldcat_totals_flat.
  clear                      g_s_fieldcat.


  add  : 1                   to  g_f_col_pos.
  move : 'END_DATE'          to  g_s_fieldcat-fieldname,
         g_f_col_pos         to  g_s_fieldcat-col_pos,
         'G_T_TOTALS_FLAT'   to  g_s_fieldcat-tabname,
         text-095            to  g_s_fieldcat-seltext_l, "from date
         text-095            to  g_s_fieldcat-seltext_m, "from date
         text-095            to  g_s_fieldcat-seltext_s, "from date
         'L'                 to  g_s_fieldcat-ddictxt,
         15                  to  g_s_fieldcat-outputlen,
         'D'                 to  g_s_fieldcat-inttype,
         'DATS'              to  g_s_fieldcat-datatype.
* fields l_ref_* are no longer needed
*         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
*         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.

  append  g_s_fieldcat       to  g_t_fieldcat_totals_flat.
  clear                      g_s_fieldcat.

*--CS2020001127 - Jaime Tassoni - 04.11.2020 - inicio
  ADD  : 1                   TO  g_f_col_pos.
  MOVE : 'LGORT'             TO  g_s_fieldcat-fieldname,
         g_f_col_pos         TO  g_s_fieldcat-col_pos,
         'G_T_TOTALS_FLAT'   TO  g_s_fieldcat-tabname,
         TEXT-140            TO  g_s_fieldcat-seltext_l, "from date
         TEXT-140            TO  g_s_fieldcat-seltext_m, "from date
         TEXT-140            TO  g_s_fieldcat-seltext_s, "from date
         'L'                 TO  g_s_fieldcat-ddictxt,
         15                  TO  g_s_fieldcat-outputlen,
         'D'                 TO  g_s_fieldcat-inttype,
         'CHAR'              TO  g_s_fieldcat-datatype.
* fields l_ref_* are no longer needed
*         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
*         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.

  APPEND  g_s_fieldcat       TO  g_t_fieldcat_totals_flat.
  CLEAR                      g_s_fieldcat.
*
  ADD  : 1                   TO  g_f_col_pos.
  MOVE : 'LGOBE'             TO  g_s_fieldcat-fieldname,
         g_f_col_pos         TO  g_s_fieldcat-col_pos,
         'G_T_TOTALS_FLAT'   TO  g_s_fieldcat-tabname,
         TEXT-141            TO  g_s_fieldcat-seltext_l, "from date
         TEXT-141            TO  g_s_fieldcat-seltext_m, "from date
         TEXT-141            TO  g_s_fieldcat-seltext_s, "from date
         'L'                 TO  g_s_fieldcat-ddictxt,
         20                  TO  g_s_fieldcat-outputlen,
         'D'                 TO  g_s_fieldcat-inttype,
         'CHAR'              TO  g_s_fieldcat-datatype.
* fields l_ref_* are no longer needed
*         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
*         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.

  APPEND  g_s_fieldcat       TO  g_t_fieldcat_totals_flat.
  CLEAR                      g_s_fieldcat.
*--CS2020001127 - Jaime Tassoni - 04.11.2020 - fim
* old logic for fields start_date and end_date
*  move : text-094            to  g_s_fieldcat-selText_l, "from date
*         'L'                 to  g_s_fieldcat-ddictxt,
*         15                  to  g_s_fieldcat-outputlen.
*  perform fc_s_flat using 'START_DATE' 'MKPF' 'BUDAT'.
*
*  move : text-095            to  g_s_fieldcat-selText_l, "to date
*         'L'                 to  g_s_fieldcat-ddictxt,
*         15                  to  g_s_fieldcat-outputlen.
*  perform fc_s_flat using 'END_DATE' 'MKPF' 'BUDAT'.
*
**********************************************************************
**** END OF NOTE 1064332                                     "n1064332
**** new logic for fields start_date & end_date              "n1064332
**********************************************************************

* Always use the text from the text symbol                   "n1333069
  move : text-096            to  g_s_fieldcat-seltext_l, "opening stock
         text-096            to  g_s_fieldcat-seltext_m,    "n1333069
         text-096            to  g_s_fieldcat-seltext_s,    "n1333069
         'L'                 to  g_s_fieldcat-ddictxt,
         23                  to  g_s_fieldcat-outputlen,
         'QUAN'              to  g_s_fieldcat-datatype,     "n1399766
         'MEINS'             to  g_s_fieldcat-qfieldname.
  move : 'QUAN'              to  g_s_fieldcat-datatype,     "n1441785
         'P'                 to  g_s_fieldcat-inttype,      "n1441785
         13                  to  g_s_fieldcat-intlen.       "n1441785
  perform fc_s_flat using 'ANFMENGE' '' ''.                 "n1333069

  move : text-097            to  g_s_fieldcat-seltext_l, "sum receipts
         'L'                 to  g_s_fieldcat-ddictxt,
         23                  to  g_s_fieldcat-outputlen,
         'MEINS'             to  g_s_fieldcat-qfieldname.
  perform fc_s_flat using 'SOLL' 'MSEG' 'MENGE'.

  move : text-098            to  g_s_fieldcat-seltext_l, "sum issues
         'L'                 to  g_s_fieldcat-ddictxt,
         23                  to  g_s_fieldcat-outputlen,
         'MEINS'             to  g_s_fieldcat-qfieldname.
  perform fc_s_flat using 'HABEN' 'MSEG' 'MENGE'.

* Always use the text from the text symbol                   "n1333069
  move : text-099            to  g_s_fieldcat-seltext_l, "end stock
         text-099            to  g_s_fieldcat-seltext_m,    "n1333069
         text-099            to  g_s_fieldcat-seltext_s,    "n1333069
         'L'                 to  g_s_fieldcat-ddictxt,
         23                  to  g_s_fieldcat-outputlen,
         'QUAN'              to  g_s_fieldcat-datatype,     "n1399766
         'MEINS'             to  g_s_fieldcat-qfieldname.   "n1333069
  move : 'QUAN'              to  g_s_fieldcat-datatype,     "n1441785
         'P'                 to  g_s_fieldcat-inttype,      "n1441785
         13                  to  g_s_fieldcat-intlen.       "n1441785
  perform fc_s_flat using 'ENDMENGE' '' ''.

  perform fc_s_flat using 'MEINS' 'MARA' 'MEINS'.

  if  bwbst = 'X'.
*   process the values, too
    move : text-100     to  g_s_fieldcat-seltext_l, "opening value
           text-100          to  g_s_fieldcat-seltext_m,    "n1333069
           text-100          to  g_s_fieldcat-seltext_s,    "n1333069
           text-100          to  g_s_fieldcat-reptext_ddic, "n3135681
           'L'               to  g_s_fieldcat-ddictxt,
           23                to  g_s_fieldcat-outputlen,
           'WAERS'           to  g_s_fieldcat-cfieldname,   "n2459328
*          'MSEG'            TO  g_s_fieldcat-ctabname,     "n3013261
*   Provide technical information for DMBTR from DD03L      "n2962179
             7               to g_s_fieldcat-intlen,        "n2962179
            'P'              to g_s_fieldcat-inttype,       "n2962179
            'CURR'           to g_s_fieldcat-datatype.      "n2962179
    perform fc_s_flat using 'ANFWERT' 'MSEG' 'DMBTR'.       "n3135681

    move : text-101     to  g_s_fieldcat-seltext_l,  "sum GR values
           'L'          to  g_s_fieldcat-ddictxt,
           23           to  g_s_fieldcat-outputlen,
           'WAERS'      to  g_s_fieldcat-cfieldname.
    perform fc_s_flat using 'SOLLWERT' 'MSEG' 'DMBTR'.

    move : text-102     to  g_s_fieldcat-seltext_l,  "sum GI values
           'L'          to  g_s_fieldcat-ddictxt,
           23           to  g_s_fieldcat-outputlen,
           'WAERS'      to  g_s_fieldcat-cfieldname.
    perform fc_s_flat using 'HABENWERT' 'MSEG' 'DMBTR'.

    move : text-103     to  g_s_fieldcat-seltext_l,   "end value
           text-103          to  g_s_fieldcat-seltext_m,    "n1333069
           text-103          to  g_s_fieldcat-seltext_s,    "n1333069
           text-103          to  g_s_fieldcat-reptext_ddic, "n3135681
           'L'               to  g_s_fieldcat-ddictxt,
           23                to  g_s_fieldcat-outputlen,
           'WAERS'           to  g_s_fieldcat-cfieldname,   "n2459328
*          'MSEG'            TO  g_s_fieldcat-ctabname,     "n3013261
*   Provide technical information for DMBTR from DD03L      "n2962179
             7               to g_s_fieldcat-intlen,        "n2962179
            'P'              to g_s_fieldcat-inttype,       "n2962179
            'CURR'           to g_s_fieldcat-datatype.      "n2962179
    perform fc_s_flat using 'ENDWERT' 'MSEG' 'DMBTR'.       "n3135681

    perform fc_s_flat using 'WAERS' 'T001' 'WAERS'.
  endif.
  if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_13\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
*
    if bwbst <> 'X'.
      move:  text-096       to  g_s_fieldcat-seltext_l,
             'L'            to  g_s_fieldcat-ddictxt,
             23             to  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   to  g_s_fieldcat-qfieldname.
      perform fc_s_flat using '/CWM/ANFMENGE' 'MSEG' '/CWM/MENGE'.
    endif.

    if bwbst <> 'X'.
      move:  text-097       to  g_s_fieldcat-seltext_l,
             'L'            to  g_s_fieldcat-ddictxt,
             23             to  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   to  g_s_fieldcat-qfieldname.
      perform fc_s_flat using '/CWM/SOLL    ' 'MSEG' '/CWM/MENGE'.
    endif.

    if bwbst <> 'X'.
      move:  text-098       to  g_s_fieldcat-seltext_l,
             'L'            to  g_s_fieldcat-ddictxt,
             23             to  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   to  g_s_fieldcat-qfieldname.
      perform fc_s_flat using '/CWM/HABEN   ' 'MSEG' '/CWM/MENGE'.
    endif.

    if bwbst <> 'X'.
      move:  text-099       to  g_s_fieldcat-seltext_l,
             'L'            to  g_s_fieldcat-ddictxt,
             23             to  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   to  g_s_fieldcat-qfieldname.
      perform fc_s_flat using '/CWM/ENDMENGE' 'MSEG' '/CWM/MENGE'.
    endif.

    if bwbst <> 'X'.
      perform fc_s_flat using '/CWM/MEINS'    'MSEG' '/CWM/MEINS'.
    endif.
* use ref to /CWM/VALUM
    if bwbst = 'X'.
      loop at g_t_fieldcat_totals_flat into  g_s_fieldcat.
        check : g_s_fieldcat-fieldname    = 'MEINS'.
        g_s_fieldcat-ref_tabname = 'MARA'.
        g_s_fieldcat-ref_fieldname = '/CWM/VALUM'.
        modify g_t_fieldcat_totals_flat from g_s_fieldcat.
        clear                g_s_fieldcat.
      endloop.
    endif.
  endif.

*  ENHANCEMENT-POINT ehp605_rm07mlbd_13 SPOTS es_rm07mlbd .

endform.                     " create_fieldcat_totals_flat.

*----------------------------------------------------------------------*
*    FC_S_FLAT
*----------------------------------------------------------------------*

form fc_s_flat     using     l_fieldname     type fieldname
                             l_ref_tabname   type ddobjname
                             l_ref_fieldname type fieldname.

  add  : 1                   to  g_f_col_pos.
  move : l_fieldname         to  g_s_fieldcat-fieldname,
         g_f_col_pos         to  g_s_fieldcat-col_pos,
         'G_T_TOTALS_FLAT'   to  g_s_fieldcat-tabname,
         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.
*  ENHANCEMENT-POINT fc_s_flat_01 SPOTS es_rm07mlbd.
  append  g_s_fieldcat       to  g_t_fieldcat_totals_flat.
  clear                      g_s_fieldcat.

endform.                     "fc_s_flat

*----------------------------------------------------------------------*
*    alv_flat_list_sums_only
*----------------------------------------------------------------------*

form alv_flat_list_sums_only.

  data: lv_lvc_s_glay type lvc_s_glay.                      "1790231

* assign the form routines to the events
  move :  'PF_STATUS_SET'         to  g_t_events_totals_flat-name,
          'PF_STATUS_SET_TOTALS'  to  g_t_events_totals_flat-form.
  append                              g_t_events_totals_flat.

  move :  'USER_COMMAND'          to  g_t_events_totals_flat-name,
          'USER_COMMAND_TOTALS'   to  g_t_events_totals_flat-form.
  append                              g_t_events_totals_flat.

  move : 'TOP_OF_PAGE'            to  g_t_events_totals_flat-name,
         'TOP_OF_PAGE_TOTALS'     to  g_t_events_totals_flat-form.
  append                              g_t_events_totals_flat.

  move :       'END_OF_LIST'      to  g_t_events_totals_flat-name,
         'PRINT_END_OF_LIST'      to  g_t_events_totals_flat-form.
  append                              g_t_events_totals_flat.

* handling for double click
  g_s_layout_totals_flat-f2code           = '9PBP'.
  g_s_layout_totals_flat-coltab_fieldname = 'COLOR'.

  if  g_flag_break-b6 = 'X'.                                "n921164
    break-point              id mmim_rep_mb5b.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  endif.                                                    "n921164

* liste aufbauen
  if gv_ui_opt_active = abap_false or p_grid = abap_false.  "1790231

    call function 'REUSE_ALV_LIST_DISPLAY'
      exporting
        i_interface_check  = g_flag_i_check
        i_callback_program = repid
        is_layout          = g_s_layout_totals_flat
        it_fieldcat        = g_t_fieldcat_totals_flat[]
        it_sort            = g_t_sorttab
        i_default          = 'X'  "allow default variant
        i_save             = 'A'
        is_variant         = g_s_vari_sumfl
        it_events          = g_t_events_totals_flat[]
        is_print           = g_s_print
      tables
        t_outtab           = g_t_totals_flat
      exceptions
        others             = 1.

  else.                                                     "1790231
    lv_lvc_s_glay-coll_top_p = abap_true.                   "1790231
    call function 'REUSE_ALV_GRID_DISPLAY'                  "1790231
      exporting                                             "1790231
        i_interface_check  = g_flag_i_check
        i_callback_program = repid
        i_grid_settings    = lv_lvc_s_glay
        is_layout          = g_s_layout_totals_flat
        it_fieldcat        = g_t_fieldcat_totals_flat[]
        it_sort            = g_t_sorttab
        i_default          = 'X'  "allow default variant
        i_save             = 'A'
        is_variant         = g_s_vari_sumfl
        it_events          = g_t_events_totals_flat[]
        is_print           = g_s_print
      tables
        t_outtab           = g_t_totals_flat
      exceptions
        others             = 1.                             "1790231
                                                            "1790231
  endif.                                                    "1790231


  if sy-subrc ne 0.
    message id sy-msgid type sy-msgty number sy-msgno
       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                     "alv_flat_list_sums_only

*----------------------------------------------------------------------*
*    create_fieldcat_totals_hq
*----------------------------------------------------------------------*
*baustelle
form create_fieldcat_totals_hq.

* create fieldcat
  clear : g_s_fieldcat,      g_f_col_pos.

* part 1 : for the header table
  if  bwbst = 'X'.                                          "n999530
    perform  fc_hq
        using   'G_T_TOTALS_HEADER' 'BWKEY'  'MBEW'  'BWKEY'.
  endif.                                                    "n999530

  if bwbst is initial.                                      "n999530
    perform  fc_hq
        using   'G_T_TOTALS_HEADER' 'WERKS'  'MARC'  'WERKS'.

    move : 'X'                 to  g_s_fieldcat-no_out.
    move : 30                  to  g_s_fieldcat-outputlen,  "n999530
           text-024            to  g_s_fieldcat-seltext_l,  "n999530
           'L'                 to  g_s_fieldcat-ddictxt.    "n999530
    perform  fc_hq
      using  'G_T_TOTALS_HEADER'  'NAME1'  'T001W'  'NAME1'.
  endif.                                                    "n999530

  perform  fc_hq
    using  'G_T_TOTALS_HEADER'  'MATNR'  'MARA'  'MATNR'.

  if  sobkz is initial.
    move : 'X'               to  g_s_fieldcat-no_out.
  endif.

  perform  fc_hq
    using  'G_T_TOTALS_HEADER'  'SOBKZ'  'MSLB'  'SOBKZ'.

  perform  fc_hq
    using  'G_T_TOTALS_HEADER'  'MAKTX'  'MAKT'  'MAKTX'.

  if  xchar is initial.
    move : 'X'               to  g_s_fieldcat-no_out.
  endif.

  perform  fc_hq
      using  'G_T_TOTALS_HEADER'  'CHARG'  'MCHB'  'CHARG'.

  if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_14\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
*
* MOVE : 'HIIER'        TO  g_s_fieldcat-seltext_l,
    if bwbst <> 'X'.
      clear: g_s_fieldcat-no_out.
      move : text-104       to  g_s_fieldcat-seltext_l,
             'L'            to  g_s_fieldcat-ddictxt,
             23             to  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   to  g_s_fieldcat-qfieldname.
      perform fc_hq
          using 'G_T_TOTALS_ITEM' '/CWM/MENGE' space space.
*      USING 'G_T_TOTALS_HEADER' '/CWM/MENGE' 'MSEG' '/CWM/MENGE'.
* Modify the last added line to positioning the field at the end of output line
      read table g_t_fieldcat_totals_hq index sy-tabix
                                      into g_s_fieldcat
                                      transporting col_pos.
      if sy-subrc = 0.
        g_s_fieldcat-col_pos = 94.
        modify g_t_fieldcat_totals_hq index sy-tabix
                                      from g_s_fieldcat
                                      transporting col_pos.
      endif.
      clear g_s_fieldcat.
    endif.

    if bwbst <> 'X'.
      perform  fc_hq
        using  'G_T_TOTALS_ITEM'  '/CWM/MEINS' 'MSEG' '/CWM/MEINS'.
*    using  'G_T_TOTALS_HEADER'  '/CWM/MEINS' 'MSEG' '/CWM/MEINS'.
* Modify the last added line to positioning the field at the end of output line
      read table g_t_fieldcat_totals_hq index sy-tabix
                                        into g_s_fieldcat
                                        transporting col_pos.
      if sy-subrc = 0.
        g_s_fieldcat-col_pos = 95.
        modify g_t_fieldcat_totals_hq index sy-tabix
                                      from g_s_fieldcat
                                      transporting col_pos.
      endif.
      clear g_s_fieldcat.
    endif.
  endif.

*  ENHANCEMENT-POINT ehp605_rm07mlbd_14 SPOTS es_rm07mlbd .

** part 2 : for the item table

* hidden key fields
  move : 'X'               to  g_s_fieldcat-no_out.
  perform  fc_hq
      using   'G_T_TOTALS_ITEM' 'BWKEY'  'MBEW'  'BWKEY'.

  move : 'X'               to  g_s_fieldcat-no_out.
  perform  fc_hq
      using   'G_T_TOTALS_ITEM' 'WERKS'  'MARC'  'WERKS'.

  move : 'X'               to  g_s_fieldcat-no_out.
  perform  fc_hq
    using  'G_T_TOTALS_ITEM'  'MATNR'  'MARA'  'MATNR'.

  move : 'X'               to  g_s_fieldcat-no_out.
  perform  fc_hq
      using  'G_T_TOTALS_ITEM'  'CHARG'  'MCHB'  'CHARG'.

  move : 'X'               to  g_s_fieldcat-no_out.
  perform  fc_hq
      using  'G_T_TOTALS_ITEM'  'COUNTER'  ' '  ''.

  move : 40                   to  g_s_fieldcat-outputlen.
  perform  fc_hq
      using  'G_T_TOTALS_ITEM'  'STOCK_TYPE' space space.

* do not allow to form sums for the column quantity         "n951316
  move : 'X'            to  g_s_fieldcat-no_sum.            "n951316
  move : 23             to  g_s_fieldcat-outputlen,
         text-104      to  g_s_fieldcat-seltext_l,  "quantities
         'L'            to  g_s_fieldcat-ddictxt,
         'MENGE_D'      to  g_s_fieldcat-rollname,
         'QUAN'         to  g_s_fieldcat-datatype,          "2288623
         'MEINS'        to  g_s_fieldcat-qfieldname.
  perform  fc_hq
    using  'G_T_TOTALS_ITEM'  'MENGE' 'MSEG' 'MENGE'.       "2288623

  perform  fc_hq
    using  'G_T_TOTALS_ITEM'  'MEINS'   'MARA'  'MEINS'.

  if  bwbst = 'X'.
*   with valuation
*   do not allow to form sums for the column value          "n951316
    move : 'X'          to  g_s_fieldcat-no_sum.            "n951316
    move : 23           to  g_s_fieldcat-outputlen,
           text-105     to  g_s_fieldcat-seltext_l,   "values
           'L'          to  g_s_fieldcat-ddictxt,
           'DMBTR'      to  g_s_fieldcat-rollname,
           'CURR'       to  g_s_fieldcat-datatype,          "2288623
           'WAERS'      to  g_s_fieldcat-cfieldname.
    perform  fc_hq
      using  'G_T_TOTALS_ITEM'  'WERT' 'MSEG' 'DMBTR'.      "2288623

    perform  fc_hq
      using  'G_T_TOTALS_ITEM'  'WAERS'   'T001'  'WAERS'.
  endif.

endform.                     "create_fieldcat_totals_hq

*----------------------------------------------------------------------*
*    FC_HQ
*----------------------------------------------------------------------*

form fc_hq         using     l_tabname        type  ddobjname
                             l_fieldname      type  fieldname
                             l_ref_tabname    type  ddobjname
                             l_ref_fieldname  type  fieldname.

  add  : 1                   to  g_f_col_pos.
  move : l_fieldname         to  g_s_fieldcat-fieldname,
         g_f_col_pos         to  g_s_fieldcat-col_pos,
         l_tabname           to  g_s_fieldcat-tabname,
         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.
  if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\FC_HQ_01\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* use ref to /CWM/VALUM
    if bwbst = 'X' and g_s_fieldcat-fieldname = 'MEINS'.
      g_s_fieldcat-ref_tabname   = 'MARA'.
      g_s_fieldcat-ref_fieldname = '/CWM/VALUM'.
    endif.
  endif.
*  ENHANCEMENT-POINT fc_hq_01 SPOTS es_rm07mlbd.

  append  g_s_fieldcat       to  g_t_fieldcat_totals_hq.
  clear                      g_s_fieldcat.

endform.                     "fc_hq

*----------------------------------------------------------------------*
* alv_hierseq_list_totals
*----------------------------------------------------------------------*

form alv_hierseq_list_totals.

* fill layout : consider double click and color subtables
  g_s_layout_totals_hq-coltab_fieldname = 'COLOR'.
  g_s_layout_totals_hq-f2code           = '9PBP'.

* create other tables and structures
  move : 'BWKEY'             to  g_s_keyinfo_totals_hq-header01,
         'BWKEY'             to  g_s_keyinfo_totals_hq-item01,

         'WERKS'             to  g_s_keyinfo_totals_hq-header02,
         'WERKS'             to  g_s_keyinfo_totals_hq-item02,

         'MATNR'             to  g_s_keyinfo_totals_hq-header03,
         'MATNR'             to  g_s_keyinfo_totals_hq-item03,

         'CHARG'             to  g_s_keyinfo_totals_hq-header04,
         'CHARG'             to  g_s_keyinfo_totals_hq-item04,

         'COUNTER'           to  g_s_keyinfo_totals_hq-item05.

* create the events table
  move : 'PF_STATUS_SET'          to  events_hierseq-name,
         'PF_STATUS_SET_TOTALS'  to  events_hierseq-form.
  append                              events_hierseq.

  move : 'USER_COMMAND'           to  events_hierseq-name,
         'USER_COMMAND_TOTALS'    to  events_hierseq-form.
  append                              events_hierseq.

  move : 'TOP_OF_PAGE'            to  events_hierseq-name,
         'TOP_OF_PAGE_TOTALS'     to  events_hierseq-form.
  append                              events_hierseq.

  move :       'END_OF_LIST'      to  events_hierseq-name,
         'PRINT_END_OF_LIST'      to  events_hierseq-form.
  append                              events_hierseq.

* create the sort table g_t_SORT_totals_hq
  clear                           g_s_sort_totals_hq.
  move : 'G_T_TOTALS_ITEM'        to  g_s_sort_totals_hq-tabname,
         'X'                      to  g_s_sort_totals_hq-up.

  move  'BWKEY'                   to  g_s_sort_totals_hq-fieldname.
  add     1                       to  g_s_sort_totals_hq-spos.
  append  g_s_sort_totals_hq      to  g_t_sort_totals_hq.

  move  'WERKS'                   to  g_s_sort_totals_hq-fieldname.
  add     1                       to  g_s_sort_totals_hq-spos.
  append  g_s_sort_totals_hq      to  g_t_sort_totals_hq.

  move  'MATNR'                   to  g_s_sort_totals_hq-fieldname.
  add     1                       to  g_s_sort_totals_hq-spos.
  append  g_s_sort_totals_hq      to  g_t_sort_totals_hq.

  move  'CHARG'                   to  g_s_sort_totals_hq-fieldname.
  add     1                       to  g_s_sort_totals_hq-spos.
  append  g_s_sort_totals_hq      to  g_t_sort_totals_hq.

  move  'COUNTER'                 to  g_s_sort_totals_hq-fieldname.
  add     1                       to  g_s_sort_totals_hq-spos.
  append  g_s_sort_totals_hq      to  g_t_sort_totals_hq.

  if  g_flag_break-b7 = 'X'.                                "n921164
    break-point              id mmim_rep_mb5b.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  endif.                                                    "n921164

  call function 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    exporting
      i_interface_check  = g_flag_i_check
      i_callback_program = repid
      it_events          = events_hierseq[]
      is_layout          = g_s_layout_totals_hq
      is_print           = g_s_print
      it_fieldcat        = g_t_fieldcat_totals_hq
      it_sort            = g_t_sort_totals_hq
      i_default          = 'X'
      i_save             = 'A'
      is_variant         = g_s_vari_sumhq
      i_tabname_header   = 'G_T_TOTALS_HEADER'
      i_tabname_item     = 'G_T_TOTALS_ITEM'
      is_keyinfo         = g_s_keyinfo_totals_hq
    tables
      t_outtab_header    = g_t_totals_header[]
      t_outtab_item      = g_t_totals_item[]
    exceptions
      others             = 1.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                     " alv_hierseq_list_totals

*-----------------------------------------------------------"n599218


*  Aus dieser Unterroutine heraus werden implizit, d.h. in der Schleife
*  über alle selektierten Bestände, die zugehörigen Materialbelege
*  aufgerufen.
*  Die Bestände werden zum Anfangs- und Enddatum als Summen zu
*  folgendem Schlüssel, der im Listkopf geführt wird, ausgegeben:
*  Buchungskreis bzw. Werk, Material, Charge. Nicht-chargenpflichtige
*  Materialien werden auf Materialebene angezeigt.
*  Es folgt jeweils eine Liste mit den einzelnen Belegpositionen.

*********************** Ende HAUPTPROGRAMM *****************************
*
************************* FORMROUTINEN *********************************

*&---------------------------------------------------------------------*
*&      Form  EINGABEN_PRUEFEN
*&---------------------------------------------------------------------*
*       Prüfung der Eingaben auf dem Selektionsbild                    *
*----------------------------------------------------------------------*
form eingaben_pruefen.

* check the entries only in releases >= 46B
  call function 'MMIM_ENTRYCHECK_MAIN'
    tables
      it_matnr = matnr
      it_werks = werks
      it_lgort = lgort
      it_bwart = bwart
      it_bukrs = bukrs.

*  Die Selektionseingaben Buchungskreis und Werk werden hierarchisch
*  verstanden, d.h. es werden nur Werke innerhalb der angegebenen
*  Buchungskreise selektiert.
*  Lagerort-/Chargenbestand: Da die Werksbezeichnung eindeutig ist,
*  finden alle Selektionen auf Werksebene bzw. - falls mindestens ein
*  Lagerort eingegeben wurde - auf der Ebene der eingegebenen Lagerorte
*  statt. Die Ausgabe erfolgt auf Werksebene des Materials / der Charge.
*  Bewerteter Bestand: Die Ausgabe erfolgt auf Bewertungskreisebene,
*  d.h. je nach Einstellung in der Tabelle TCURM auf Werks- oder
*  Buchungskreisebene.

*  Feststellen, ob der Bewertungskreis auf Buchungskreis- oder
*  Werksebene liegt:
*  tcurm-bwkrs_cus = 1  =>  Bewertungskreis auf Werksebene,
*  tcurm-bwkrs_cus = 3  =>  Bewertungskreis auf Buchungskreisebene.
  select bwkrs_cus from tcurm into curm
            order by primary key.
  endselect.

  if xchar = ' ' and not charg-low is initial.
    xchar = 'X'.
  endif.
  if xchar = ' ' and not xnomchb is initial.                "838360_v
    xchar = 'X'.
  endif.                                                    "838360_^

  if sbbst = 'X' and sobkz is initial.
    message e286.
*   Bitte ein Sonderbestandskennzeichen eingeben.
  elseif sbbst = ' ' and not sobkz is initial.
    clear sobkz.
    message w287.
*   Sonderbestandskennzeichen wird zurückgesetzt.
  endif.

* reset the entries for plant when valuation area is        "n599218
* company code and mode is valuated stock                   "n599218
  if     curm  = '3'      and                               "n599218
         bwbst = 'X'.                                       "n599218
    if  not werks[]  is initial.                            "n599218
*     reset the restricts for plants                        "n599218
      clear                  werks.                         "n599218
      refresh                werks.                         "n599218
*     text-074 : valuation area = company code              "n599218
*     text-075 : entries for plant will be reset            "n599218
      message w010(ad) with text-074 text-075 space space.  "n599218
    endif.                                                  "n599218
  endif.                                                    "n599218

  if bwbst = 'X' and not charg is initial
    or bwbst = 'X' and not xchar is initial.
    clear charg. refresh charg.
    message w285.
*   Charge wird zurückgesetzt.
  endif.
  if bwbst = 'X' and not lgort is initial.
    clear lgort. refresh lgort.
    message w284.
*   Lagerort wird zurückgesetzt.
  endif.

* consider and prepare select-options depending on the required
* special stock indicator
  refresh                    g_ra_sobkz.
  clear                      g_ra_sobkz.

  if      lgbst = 'X'.       "only Storage loc./batch stock
*   create ranges table : select only sobkz = space
    perform f0500_append_ra_sobkz   using  c_space.

  elseif  bwbst = 'X'.       "only valuated stocks
*   take all special stock indicators / the record selection
*   will be done after the database selection

  elseif  sbbst = 'X'.       "only special stocks
    perform f0500_append_ra_sobkz   using  sobkz.

*ENHANCEMENT-SECTION     rm07mlbd_02 SPOTS es_rm07mlbd.

    "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
* DI A&D SSP
    if      sobkz  =  'O'  or
            sobkz  =  'V'  or
            sobkz  =  'W'  or
            sobkz  =  'E'  or
            sobkz  =  'K'  or
            sobkz  =  'M'  or
            sobkz  =  'Q'  or
            sobkz  =  'T'  or
* DI IS-ADEC-SSP  Customer Stock for DI or DIMP User
            sobkz  =  'B' or
* A&D IS-ADEC-SUB Sales Order Stock, Project Stock,       "v_n_GA1551829
* Customer Stock, Vendor Consignment or RTP Stock
            sobkz  =  cl_adsub_constants=>c  or
            sobkz  =  cl_adsub_constants=>f  or
            sobkz  =  cl_adsub_constants=>i  or
            sobkz  =  cl_adsub_constants=>j  or
            sobkz  =  cl_adsub_constants=>r.              "^_n_GA1551829
*            sobkz  =  cl_adsub_constants=>t.       "SIT SPAU "GA1990678

*     ok; no aktion taken
    else.
      set cursor             field  'SOBKZ'.
*     Sonderbestandskennzeichen nicht vorhanden
      message                e221.
    endif.
    "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*END-ENHANCEMENT-SECTION.
  endif.

  if bwbst = 'X' and not bwart is initial.
    clear bwart. refresh bwart.
    message w298.
*   Bewegungsart wird zurückgesetzt
  endif.
  if bwbst = ' ' and not bwtar is initial.
    clear bwtar. refresh bwtar.
    message w288.
*   Bewertungsart wird zurückgesetzt.
  endif.

  if gv_switch_ehp6ru = abap_true and not hkont[] is initial.
    if bwbst = ' '.
*     G/L account will be reset, if stock type is not Valuated Stock
      clear hkont. refresh hkont.
      message w481.
    else.
*     Company code or plant should be filled to build G_T_ORGAN table
      if bukrs[] is initial and werks[] is initial.
        set cursor field 'HKONT-LOW'.
        message e480.
      endif.
    endif.
  endif.

* The function "no cancellations" is not possible
* for valuated stock
*   for the selection of the reversal movements only in release >=45B
  if nosto = 'X' and bwbst = 'X'.                           "204463
    message e151(e1) with 'VALUATED_STOCK'                  "204463
                       'NO_CANCELLATIONS'.                  "204463
  endif.                                                    "204463

  if not p_vari is initial.
    move variante to def_variante.
    move p_vari to def_variante-variant.

    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = variant_save
      changing
        cs_variant = def_variante.
    variante = def_variante.
  else.
*   the user wants no initial display variant               "n599218
    if  not alv_default_variant  is initial.                "n599218
*     but the SAP-LIST-VIEWER will apply the existing       "n599218
*     initial display variant / emerge warning 393 ?        "n599218
      call function 'ME_CHECK_T160M'               "n599218
        exporting                                           "n599218
          i_arbgb = 'M7'                         "n599218
          i_msgnr = '393'                        "n599218
        exceptions                                          "n599218
          nothing = 0                            "n599218
          others  = 1.                           "n599218
                                                            "n599218
      if sy-subrc <> 0.                                     "n599218
*       list will be created using the initial layout &     "n599218
        message w393(m7)     with  alv_default_variant.     "n599218
      endif.                                                "n599218
    endif.                                                  "n599218

    clear variante.
    variante-report = repid.
  endif.

endform.                               " EINGABEN_PRÜFEN

*----------------------------------------------------------------------*
*    VARIANT_CHECK_EXISTENCE
*----------------------------------------------------------------------*

form variant_check_existence
         using  l_vari       like  disvariant-variant
                ls_vari      like  disvariant
                ls_vari_def  like  disvariant.


  move  l_vari               to  ls_vari-variant.

  if  not l_vari is initial.
*   parameter for the variant is filled.

    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = 'A'
      changing
        cs_variant = ls_vari.

*   in the case the variant does not exist this function
*   module sends the error message directly
  else.
*   the user wants no initial display variant
    if  not ls_vari_def-variant  is initial.
*     but the SAP-LIST-VIEWER will apply the existing       "n599218
*     initial display variant / emerge warning 393 ?        "n599218
      call function 'ME_CHECK_T160M'               "n599218
        exporting                                           "n599218
          i_arbgb = 'M7'                         "n599218
          i_msgnr = '393'                        "n599218
        exceptions                                          "n599218
          nothing = 0                            "n599218
          others  = 1.                           "n599218
                                                            "n599218
      if sy-subrc <> 0.                                     "n599218
*       list will be created using the initial layout &     "n599218
        message w393(m7)     with  ls_vari_def-variant.     "n599218
      endif.                                                "n599218
    endif.                                                  "n599218
  endif.

endform.                     "VARIANT_CHECK_EXISTENCE

*----------------------------------------------------------------------*
*    get_the_default_VARIANT
*----------------------------------------------------------------------*

form get_the_default_variant
         using     l_vari      like  disvariant-variant
                   ls_vari     like  disvariant
                   ls_vari_def like  disvariant.

  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save     = variant_save
    changing
      cs_variant = ls_vari_def
    exceptions
      not_found  = 2.

  if sy-subrc = 0.
*   save the initial, e.g. default variant
    move-corresponding ls_vari_def     to  ls_vari.
    move : ls_vari_def-variant         to  l_vari.
  endif.

endform.                     "VARIANT_VALUE_REQUEST_F4

*----------------------------------------------------------------------*
*    VARIANT_VALUE_REQUEST_F4
*----------------------------------------------------------------------*

form variant_value_request_f4
         using     l_vari    like  disvariant-variant
                   ls_vari   like  disvariant.

  data : ls_vari_return      like  disvariant.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = ls_vari
      i_save     = 'A'
*     it_default_fieldcat =
    importing
      e_exit     = variant_exit
      es_variant = ls_vari_return
    exceptions
      not_found  = 2.

  if sy-subrc = 2.
    message id sy-msgid type 'S' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    if variant_exit = space.
      move  ls_vari_return-variant   to  l_vari.
    endif.
  endif.

endform.                     "VARIANT_VALUE_REQUEST_F4

*&---------------------------------------------------------------------*

*---------------------- bewerteter Bestand ----------------------------*

form aktuelle_bst_bwbst.

* define local working areas  / for the result of the       "n450764
* database selections and the control break                 "n450764
  data : l_t_mbew         type  stab_mbew,                  "n450764
         l_s_mbew         type  stype_mbew,                 "n450764
                                                            "n450764
         l_s_mbew_split   type  stype_mbew,                 "n450764
         l_s_mbew_normal  type  stype_mbew,                 "n450764
         l_flag_split(01) type c.                           "n450764
                                                            "n450764

  if gv_switch_ehp6ru = abap_true and hkont is not initial.
    perform build_bklas_selection using l_t_mbew.

  else.
*   read the matching valuation entries                       "n450764
    perform hdb_check_table using 'MBEW' ''.                "n1710850
    select matnr bwkey bwtar lbkum salk3 bwtty  from mbew connection (dbcon) "n1710850
           into corresponding fields of table l_t_mbew      "n450764
           where  matnr  in  matnr                          "n450764
             and  bwkey  in  g_ra_bwkey                     "n450764
             and  bwtar  in  bwtar.                         "n450764
                                                            "n450764
                                                            "n450764
*   read the matching valuation records of the valuated       "n450764
*   special stock sales order                                 "n450764
    perform hdb_check_table using 'EBEW' ''.                "n1710850
    select matnr bwkey bwtar bwtty                          "n1227439
           sum( lbkum ) as lbkum                            "n450764
           sum( salk3 ) as salk3        from  ebew connection (dbcon) "n1710850
           appending corresponding fields of table l_t_mbew "n450764
           where  matnr  in  matnr                          "n450764
             and  bwkey  in  g_ra_bwkey                     "n450764
             and  bwtar  in  bwtar                          "n450764
    group by  matnr  bwkey bwtar bwtty.                     "n450764
                                                            "n450764
*   read the matching valuation records of the valuated       "n450764
*   special stock projects                                    "n450764
    perform hdb_check_table using 'QBEW' ''.                "n1710850
    select matnr bwkey bwtar bwtty                          "n1227439
           sum( lbkum ) as lbkum                            "n450764
           sum( salk3 ) as salk3        from  qbew connection (dbcon) "n1710850
           appending corresponding fields of table l_t_mbew "n450764
           where  matnr  in  matnr                          "n450764
             and  bwkey  in  g_ra_bwkey                     "n450764
             and  bwtar  in  bwtar                          "n450764
    group by  matnr  bwkey bwtar bwtty.                     "n450764

*   read the matching valuation records of the valuated       "n497992
*   special subcontractor stock OBEW                          "n497992
    perform hdb_check_table using 'OBEW' ''.                "n1710850
    select matnr bwkey bwtar bwtty                          "n1227439
           sum( lbkum ) as lbkum                            "n497992
           sum( salk3 ) as salk3         from  obew connection (dbcon) "n1710850
           appending corresponding fields of table l_t_mbew "n497992
           where  matnr  in  matnr                          "n497992
             and  bwkey  in  g_ra_bwkey                     "n497992
             and  bwtar  in  bwtar                          "n497992
    group by  matnr  bwkey bwtar bwtty.                     "n497992




  endif.


  if l_t_mbew[] is initial.                                 "n1560727
    message s289.                                           "n1560727
*   Kein Material in Selektion vorhanden.                      "n1560727
    perform anforderungsbild.                               "n1560727
  endif.                                                    "n1560727

* create table g_t_organ if it is still empty
  if  g_t_organ[] is initial.                               "n433765
*   create working table G_0000_RA_BWKEY with the valuation areas
    loop at l_t_mbew         into  l_s_mbew.                "n450764
      on change of l_s_mbew-bwkey.                          "n450764
        move : l_s_mbew-bwkey                               "n450764
                             to  g_0000_ra_bwkey-low,       "n450764
               'I'           to  g_0000_ra_bwkey-sign,      "n450764
               'EQ'          to  g_0000_ra_bwkey-option.    "n450764
        collect              g_0000_ra_bwkey.               "n450764
      endon.                                                "n450764
    endloop.

    perform  f0000_create_table_g_t_organ
                             using  c_no_error.
  endif.

  sort  l_t_mbew             by  matnr  bwkey.              "n450764
                                                            "n450764
  loop at l_t_mbew           into  l_s_mbew.                "n450764
*   check if MBEW record is a mother segment (splitval)     "n1227439
    if  l_s_mbew-bwtar is initial                           "n1227439
        and not l_s_mbew-bwtty is initial.                  "n1227439
      clear l_s_mbew-lbkum.                                 "n1227439
      clear l_s_mbew-salk3.                                 "n1227439
    endif.                                                  "n1227439
*   process a single entry / add the stock and value        "n450764
    if  l_s_mbew-bwtar is initial.                          "n450764
      move : l_s_mbew-matnr  to  l_s_mbew_normal-matnr,     "n450764
             l_s_mbew-bwkey  to  l_s_mbew_normal-bwkey.     "n450764
      add :  l_s_mbew-lbkum  to  l_s_mbew_normal-lbkum,     "n450764
             l_s_mbew-salk3  to  l_s_mbew_normal-salk3.     "n450764
    else.                                                   "n450764
*     material has split valuation                          "n450764
      move : 'X'             to  l_flag_split,              "n450764
             l_s_mbew-matnr  to  l_s_mbew_split-matnr,      "n450764
             l_s_mbew-bwkey  to  l_s_mbew_split-bwkey.      "n450764
      add :  l_s_mbew-lbkum  to  l_s_mbew_split-lbkum,      "n450764
             l_s_mbew-salk3  to  l_s_mbew_split-salk3.      "n450764
    endif.                                                  "n450764
                                                            "n450764
*   control break after material and valuation area         "n450764
    at end of bwkey.                                        "n450764
*     create a entry for the next working table             "n450764
      if  l_flag_split = 'X'.                               "n450764
*       if the material has split valuation, take only      "n450764
*       the sums from the entries with valuation type       "n450764
        move-corresponding  l_s_mbew_split  to  g_s_mbew.   "n450764
      else.                                                 "n450764
        move-corresponding  l_s_mbew_normal to  g_s_mbew.   "n450764
      endif.                                                "n450764
                                                            "n450764
*     check the authority                                   "n450764
      perform  f9300_read_organ                             "n450764
                   using     c_bwkey   g_s_mbew-bwkey.      "n450764
                                                            "n450764
      if sy-subrc is initial.                               "n450764
*       enrich the entries with the field currency key      "n450764
        move g_s_organ-waers to  g_s_mbew-waers.            "n450764
        append  g_s_mbew     to  g_t_mbew.                  "n450764
                                                            "n450764
*       create the key table for the material texts         "n450764
        perform  f9400_material_key                         "n450764
                             using  g_s_mbew-matnr.         "n450764
      endif.                                                "n450764
                                                            "n450764
*     clear the working areas for the next group            "n450764
      clear : l_flag_split, l_s_mbew_normal, l_s_mbew_split. "n450764
    endat.                                                  "n450764
  endloop.                                                  "n450764

* no entries left in table g_t_mbew ?
  if  g_t_mbew[] is initial.                                "n450764
    message s289.
*     Kein Material in Selektion vorhanden.
    perform anforderungsbild.
  endif.

endform.                     "aktuelle_bst_bwbst

*&---------------------------------------------------------------------*
*&      Form  BEWEGUNGSARTEN_LESEN
*&---------------------------------------------------------------------*
*       Lesen der Tabellen zur Bewegungsart                            *
*----------------------------------------------------------------------*

form  bewegungsarten_lesen.

  data: begin of k2 occurs 0,
          bwart like t156s-bwart,
        end of k2.
  refresh k2.

* select the movement types from the selected documents
  loop at g_t_mseg_lean      into  g_s_mseg_lean.
    move  g_s_mseg_lean-bwart          to  k2-bwart.
    collect                            k2.
  endloop.

* Read data for movement type from new tables
* T156SY/C/Q with function module from release >=46B
  data: t_st156s         like st156s occurs 0
  with header line.

  refresh it156.

* optimized movement type selection                           "v2724521
  select t156~bwart t156sy~wertu t156sy~mengu
         t156sy~sobkz t156sy~kzbew t156sy~kzzug t156sy~kzvbr
         t156sy~bustm t156sy~bustw t156q~bwagr
     into corresponding fields of table it156
      from t156sy
       join t156
        on  t156~bustr  = t156sy~bustr
       join t156q
        on  t156q~bwart = t156~bwart
        and t156q~sobkz = t156sy~sobkz
        and t156q~kzbew = t156sy~kzbew
        and t156q~kzzug = t156sy~kzzug
        and t156q~kzvbr = t156sy~kzvbr
       for all entries in k2 where t156~bwart = k2-bwart. "#EC CI_BUFFJOIN

  sort it156 by bwart wertu mengu sobkz kzbew kzzug kzvbr.

* LBBSA is not used, skipped selection of T156M               "^2724521

  data: rc type i.                                          "147374
  loop at g_t_mseg_lean      into  g_s_mseg_lean.
*   find and delete reversal movements / only in releases >= 45B
    if not nosto is initial and
       not ( g_s_mseg_lean-smbln is initial or
             g_s_mseg_lean-smblp is initial ).
      move-corresponding  g_s_mseg_lean
                             to  storno.

      append storno.
      delete                 g_t_mseg_lean.
      continue.
    endif.

    read table it156 with key bwart = g_s_mseg_lean-bwart
                              wertu = g_s_mseg_lean-wertu
                              mengu = g_s_mseg_lean-mengu
                              sobkz = g_s_mseg_lean-sobkz
                              kzbew = g_s_mseg_lean-kzbew
                              kzzug = g_s_mseg_lean-kzzug
                              kzvbr = g_s_mseg_lean-kzvbr
                             binary search.

    rc = sy-subrc.                                          "147374
    if  g_s_mseg_lean-bustm = space and
        g_s_mseg_lean-bustw = space and
        rc                  = 0.                            "147374
      move : it156-bustm     to  g_s_mseg_lean-bustm,       "147374
             it156-bustw     to  g_s_mseg_lean-bustw.       "147374
    endif.

    if rc = 0.                                              "147374
      move : it156-lbbsa     to  g_s_mseg_lean-lbbsa.

      if not it156-bwagr is initial.
        move : it156-bwagr   to  g_s_mseg_lean-bwagr.
      else.
        move : 'REST'        to  g_s_mseg_lean-bwagr.
      endif.
    else.
      move : 'REST'          to  g_s_mseg_lean-bwagr.
    endif.                                                  "147374

    modify  g_t_mseg_lean    from  g_s_mseg_lean.
  endloop.

endform.                    " BEWEGUNGSARTEN_LESEN

*&---------------------------------------------------------------------*
*&      Form  SUMMEN_BILDEN
*&---------------------------------------------------------------------*
*       Bestandssummen zur Berechnung der Bestände                     *
*       zu 'datum-low' und 'datum-high'                                *
*----------------------------------------------------------------------*
form summen_bilden.
* Some explanatory words on the strategy of material
* counting/valuation:
* ======================================================
* 1) Stock overview (no valuation):
*    The material document is accepted, if is has not been created
*    automatically or if it is not related to movements out of
*    the stock. For example, if a stock transfer is posted, the
*    system creates a material document with two lines: Out of
*    the old stock (accepted) and into the transfer stock (rejected,
*    because the material is not yet visible in the target location).
*    When the movement into the stock is posted, this is accepted.
* 2) Valuated stock:
*    a) Movements within a single plant (MA05, MA06 =
*       movement types 313-316) are ignored.
*    b) The moving of material out of a plant (303/304)
*       is counted and valuated in the emitting plant and
*       the target plant. The moving in
*       (305/306) is ignored, because
*       the valuated stock appears in the target at the
*       very moment of leaving the emitter.
*    c) Material documents without valuation string are ignored.
*------------- Summen von 'datum-high' bis Gegenwart ------------------*
* Performance Optimization:                                 "1784986
* Form is called from FORM create_table_for_detail!         "1784986
  if gv_newdb = abap_true.                                  "1784986
    delete g_t_mseg_lean where                              "1784986
             ( xauto is not initial ) and                   "1784986
             ( bustm = 'MA02' or                            "1784986
               bustm = 'MA05' or                            "1784986
               bustm = 'MAUO' or                            "1784986
               bustm = 'MA0L' or                            "1784986
               bustm = 'MAVA' ).                            "1810543
    return.                                                 "1784986
  endif.                                                    "1784986
  if not index_2 is initial.
    if bwbst = ' '.
      if xchar = ' '.
        sort imsweg by werks matnr shkzg.          "auf Materialebene
        loop at imsweg.
*ENHANCEMENT-SECTION rm07mlbd_20 SPOTS es_rm07mlbd .

          "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
          if ( imsweg-xauto is initial ) or
             ( imsweg-bustm <> 'MA02' and
               imsweg-bustm <> 'MA05' and
               imsweg-bustm <> 'MAUO' and                   "1767021
               imsweg-bustm <> 'MA0L' and                   "1767021
               imsweg-bustm <> 'MAVA' and                   "1835358
               imsweg-bustm <> 'MXCB' and           "SH note 1728394
               imsweg-bustm <> 'MXCE' and           "SH note 1728394
               imsweg-bustm <> 'MXAK' and           "SH note 1728394
               imsweg-bustm <> 'MXCQ' and           "SH note 1728394
               imsweg-bustm <> '' ).
            move-corresponding imsweg to weg_mat.
            if not imsweg-lgort is initial
               and ( imsweg-sobkz = 'W'
                 or  imsweg-sobkz = 'V'
                 or  imsweg-sobkz = 'O' ).
              clear weg_mat-lgort.
            endif.
            collect weg_mat.
          else.
            delete imsweg.
          endif.
          "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*END-ENHANCEMENT-SECTION.
        endloop.
      elseif xchar = 'X'.
        sort imsweg by werks matnr charg shkzg.    "auf Chargenebene
        loop at imsweg.
*ENHANCEMENT-SECTION rm07mlbd_21 SPOTS es_rm07mlbd .

          "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
          if ( imsweg-xauto is initial ) or
             ( imsweg-bustm <> 'MA02' and
               imsweg-bustm <> 'MA05' and
               imsweg-bustm <> 'MXCB' and           "SH note 1728394
               imsweg-bustm <> 'MXCE' and           "SH note 1728394
               imsweg-bustm <> 'MXAK' and           "SH note 1728394
               imsweg-bustm <> 'MXCQ' and           "SH note 1728394
               imsweg-bustm <> 'MAUO' and                   "1767021
               imsweg-bustm <> 'MA0L' and                   "1767021
               imsweg-bustm <> 'MAVA'   ).                  "1835358
            move-corresponding imsweg to weg_char.
            if not imsweg-lgort is initial
               and ( imsweg-sobkz = 'W'
                 or  imsweg-sobkz = 'V'
                 or  imsweg-sobkz = 'O' ).
              clear weg_char-lgort.
            endif.
            collect weg_char.
          else.
            delete imsweg.
          endif.
          "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*END-ENHANCEMENT-SECTION.
        endloop.
      endif.

    elseif bwbst = 'X'.
      sort imsweg by werks matnr shkzg.
      loop at imsweg.
*       consider special gain/loss-handling of IS-OIL       "n497992

**# IF EXIST OI001
**" IF ( imsweg-bustm <> 'MEU1' )    OR                 "n497992
**"   ( imsweg-bustm = 'MEU1'                           "n497992
**"   AND not imsweg-OIGLCALC IS INITIAL                "n497992
**"   AND not imsweg-OIGLSKU IS INITIAL ).              "n497992
**"          MOVE-CORRESPONDING imsweg TO mat_weg.      "n497992
**"          COLLECT mat_weg.                           "n497992
**" ELSE.                                               "n497992
**"          DELETE             imsweg.                 "n497992
**" ENDIF .                                             "n497992
**# ELSE
*     MOVE-CORRESPONDING imsweg TO mat_weg.             "n497992
*     COLLECT mat_weg.                                  "n497992
**# ENDIF
*       IS-OIL specific functions without ABAP preprocessor "n599218 A
        if  g_flag_is_oil_active = 'X'.     "IS-OIL ?       "n599218 A
          if ( imsweg-bustm <> 'MEU1' )    or               "n599218 A
             ( imsweg-bustm = 'MEU1'                        "n599218 A
               and not imsweg-oiglcalc is initial           "n599218 A
               and not imsweg-oiglsku is initial ).         "n599218 A
            move-corresponding imsweg to mat_weg.           "n599218 A
            collect mat_weg.                                "n599218 A
          else.                                             "n599218 A
            delete           imsweg.                        "n599218 A
          endif.                                            "n599218 A
        else.                                               "n599218 A
          move-corresponding imsweg to mat_weg.             "n599218 A
          collect mat_weg.                                  "n599218 A
        endif.                                              "n599218 A

      endloop.

      loop at mat_weg.
        if curm = '1'.
          mat_weg-bwkey = mat_weg-werks.
        elseif curm = '3'.
*
*         look for the corresponding valuation area
*         READ TABLE organ WITH KEY werks = mat_weg-werks.
*         mat_weg-bwkey = organ-bwkey.
          perform  f9300_read_organ
                   using     c_werks   mat_weg-werks.

          move : g_s_organ-bwkey   to  mat_weg-bwkey.
        endif.
        modify mat_weg.
      endloop.
      if curm = '3'.
        sort mat_weg by bwkey matnr shkzg.
        loop at mat_weg.
          move-corresponding mat_weg to mat_weg_buk.
          collect mat_weg_buk.
        endloop.
      endif.
    endif.
  endif.

*------------- Summen von 'datum-low' bis 'datum-high' ----------------*
  if bwbst = ' '.
    if xchar = ' '.                    "auf Materialebene

      sort  g_t_mseg_lean    by werks matnr shkzg descending.

      loop at g_t_mseg_lean  into  g_s_mseg_lean.
*ENHANCEMENT-SECTION rm07mlbd_22 SPOTS es_rm07mlbd .

        "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
        if ( g_s_mseg_lean-xauto is initial ) or
           ( g_s_mseg_lean-bustm <> 'MA02' and
             g_s_mseg_lean-bustm <> 'MA05' and
             g_s_mseg_lean-bustm <> 'MXCB' and           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXCE' and           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXAK' and           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXCQ' and           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MAUO' and              "1767021
             g_s_mseg_lean-bustm <> 'MA0L' and              "1767021
             g_s_mseg_lean-bustm <> 'MAVA'   ).             "1835358
          move-corresponding g_s_mseg_lean   to  sum_mat.
          collect            sum_mat.
        else.
          delete             g_t_mseg_lean.
        endif.
        "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*END-ENHANCEMENT-SECTION.
      endloop.

    elseif xchar = 'X'.                "auf Chargenebene
      sort  g_t_mseg_lean    by werks matnr charg shkzg descending.

      loop at g_t_mseg_lean  into  g_s_mseg_lean.
*ENHANCEMENT-SECTION rm07mlbd_23 SPOTS es_rm07mlbd .

        "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
        if ( g_s_mseg_lean-xauto is initial ) or
           ( g_s_mseg_lean-bustm <> 'MA02' and
             g_s_mseg_lean-bustm <> 'MA05' and
             g_s_mseg_lean-bustm <> 'MXCB' and           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXCE' and           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXAK' and           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXCQ' and           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MAUO' and              "1767021
             g_s_mseg_lean-bustm <> 'MA0L' and              "1767021
             g_s_mseg_lean-bustm <> 'MAVA'   ).             "1835358
          move-corresponding  g_s_mseg_lean
                             to  sum_char.
          collect            sum_char.
        else.
          delete             g_t_mseg_lean.
        endif.
        "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*END-ENHANCEMENT-SECTION.
      endloop.
    endif.

  elseif bwbst = 'X'.
    sort  g_t_mseg_lean      by werks matnr shkzg descending.
    loop at g_t_mseg_lean    into  g_s_mseg_lean.
*       consider special gain/loss-handling of IS-OIL       "n497992

**# IF EXIST OI001
**"    IF ( G_S_MSEG_LEAN-bustm <> 'MEU1' )    OR       "n497992
**"       ( G_S_MSEG_LEAN-bustm = 'MEU1'                "n497992
**"       AND not G_S_MSEG_LEAN-OIGLCALC IS INITIAL     "n497992
**"       AND not G_S_MSEG_LEAN-OIGLSKU IS INITIAL ).   "n497992
**# ENDIF
*      MOVE-CORRESPONDING  G_S_MSEG_LEAN
*                             TO  MAT_SUM.
*      COLLECT                MAT_SUM.
**# IF EXIST OI001
**"    ELSE.                                            "n497992
**"      DELETE               G_T_MSEG_LEAN.            "n497992
**"    ENDIF.                                           "n497992
**# ENDIF
*     IS-OIL specific functions without ABAP preprocessor   "n599218 A
      if  g_flag_is_oil_active = 'X'.       "IS-OIL ?       "n599218 A
        if ( g_s_mseg_lean-bustm <> 'MEU1' )    or          "n599218 A
           ( g_s_mseg_lean-bustm = 'MEU1'                   "n599218 A
           and not g_s_mseg_lean-oiglcalc is initial        "n599218 A
           and not g_s_mseg_lean-oiglsku is initial ).      "n599218 A
          move-corresponding  g_s_mseg_lean                 "n599218 A
                             to  mat_sum.                   "n599218 A
          collect            mat_sum.                       "n599218 A
        else.                                               "n599218 A
          delete             g_t_mseg_lean.                 "n599218 A
        endif.                                              "n599218 A
      else.                                                 "n599218 A
        move-corresponding  g_s_mseg_lean                   "n599218 A
                             to  mat_sum.                   "n599218 A
        collect              mat_sum.                       "n599218 A
      endif.                                                "n599218 A
    endloop.

    loop at mat_sum.
      if curm = '1'.
        mat_sum-bwkey = mat_sum-werks.
      elseif curm = '3'.
        perform  f9300_read_organ
                   using     c_werks   mat_sum-werks.

        move : g_s_organ-bwkey     to  mat_sum-bwkey.
      endif.
      modify mat_sum.
    endloop.

    if curm = '3'.            "Materialbelege auf Buchungskreisebene
      sort mat_sum by bwkey matnr shkzg descending.
      loop at mat_sum.
        move-corresponding mat_sum to mat_sum_buk.
        collect mat_sum_buk.
      endloop.
    endif.
  endif.

endform.                               " SUMMEN_BILDEN

*&---------------------------------------------------------------------*
*&      Form  BELEGSELEKTION
*&---------------------------------------------------------------------*

form belegselektion.

* does the user wants the valuated stocks ?
  if bwbst = 'X'.
*   delete the MM-documents without values
    perform unbewertet_weg.

    if g_t_mbew[] is initial.                               "n450764
      message s289.
*     Kein Material in Selektion vorhanden.
      perform anforderungsbild.
    endif.

*   select the corresponding FI-documents
    perform                  fi_belege_lesen.
  endif.

  if sbbst is initial.
    perform                  kontiert_aussortieren.
  else.                                                     "2120566
    perform                  wesperr_aussortieren.          "2120566
  endif.

  perform                    bewegungsarten_lesen.

* does the user want no reversal movements ? only in releases >=45B
  if not nosto is initial.
    perform                  storno.
  endif.

* does the user wants the valuated stocks ?
  if bwbst = 'X'.
    perform                  belege_ergaenzen.
  endif.

  perform                    belege_sortieren.

endform.                     "BELEGSELEKTION

*&--------------------------------------------------------------------*
*&   FELDGRUPPEN_AUFBAUEN
*&--------------------------------------------------------------------*
*& create table GRUPPEN with the parameter for special groups         *
*&--------------------------------------------------------------------*

form feldgruppen_aufbauen.

* Gruppendefinitionen Positionsfelder
  gruppen-sp_group = 'M'.
  gruppen-text = text-050.
  append gruppen.
  gruppen-sp_group = 'B'.
  gruppen-text = text-051.
  append gruppen.
  gruppen-sp_group = 'D'.
  gruppen-text = text-052.
  append gruppen.
  gruppen-sp_group = 'O'.
  gruppen-text = text-053.
  append gruppen.
  gruppen-sp_group = 'K'.
  gruppen-text = text-054.
  append gruppen.
  gruppen-sp_group = 'E'.
  gruppen-text = text-055.
  append gruppen.
  gruppen-sp_group = 'V'.
  gruppen-text = text-056.
  append gruppen.
  gruppen-sp_group = 'F'.
  gruppen-text = text-057.
  append gruppen.
  gruppen-sp_group = 'S'.
  gruppen-text = text-058.
  append gruppen.
  layout-group_buttons = ' '.

endform.                               " FELDGRUPPEN_AUFBAUEN.

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT
*&---------------------------------------------------------------------*

form ueberschrift.                                          "#EC CALLED

  move-corresponding  bestand
                             to  g_s_bestand.
  perform                    top_of_page_render.

endform.                               " UEBERSCHRIFT

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT1
*&---------------------------------------------------------------------*

form ueberschrift1.                                         "#EC CALLED

  move-corresponding  bestand1
                             to  g_s_bestand.
  perform                    top_of_page_render.

endform.                               " UEBERSCHRIFT1

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT_DETAIL
*&---------------------------------------------------------------------*

form ueberschrift_detail.                                   "#EC CALLED

  move-corresponding  g_s_bestand_detail
                             to  g_s_bestand.

  perform                    top_of_page_render.

endform.                               " UEBERSCHRIFT_DETAIL

*&---------------------------------------------------------------------*
*&      Form  STORNO
*&---------------------------------------------------------------------*
*       Stornobewegungen vernachlässigen
*----------------------------------------------------------------------*

* delete the reversal movements from the working
* table with the documents / only in releases >=45B
form storno.

  loop at storno.
    delete g_t_mseg_lean
             where mblnr = storno-smbln                     "204463
               and mjahr = storno-sjahr                     "204463
               and zeile = storno-smblp.                    "204463
  endloop.

endform.                   " STORNO

*----------------------------------------------------------------------*
* F0400_CREATE_FIELDCAT
*----------------------------------------------------------------------*
*
* create field catalog for the ALV
* take only the field of structure MSEG_LEAN who are in working
* table g_f_mseg_fields

* --> input    name of ALV input data table
* <-- output   table wilh the field catalog
*
*----------------------------------------------------------------------*

form f0400_create_fieldcat.

  clear                      g_s_fieldcat.

* lagerort                   storage location
* the following special stocks O, V, W need no storage location
  if  sobkz = 'O'  or
      sobkz = 'V'  or
      sobkz = 'W'.
  else.
    g_s_fieldcat-fieldname     = 'LGORT'.
    g_s_fieldcat-ref_tabname   = 'MSEG'.
    g_s_fieldcat-sp_group      = 'O'.
    perform  f0410_fieldcat    using  c_take   c_out.
  endif.

* Bewegungsart               movement type
  g_s_fieldcat-fieldname     = 'BWART'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_out.

* Sonderbestandskennzeichen  Special stock indicator
  g_s_fieldcat-fieldname     = 'SOBKZ'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_take   c_out.

* Nummer des Materialbelegs  Number of material document
  g_s_fieldcat-fieldname     = 'MBLNR'.
  g_s_fieldcat-ref_tabname   = 'MKPF'.
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_take   c_out.

* Position im Materialbeleg  Item in material document
  g_s_fieldcat-fieldname     = 'ZEILE'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_take   c_out.

  if bwbst = 'X'.
*   Nummer Buchhaltungsbeleg   Accounting document number
    g_s_fieldcat-fieldname     = 'BELNR'.
    g_s_fieldcat-ref_tabname   = 'BSIM'.
    g_s_fieldcat-sp_group      = 'O'.
    perform  f0410_fieldcat    using  c_take   c_out.
  endif.

* Buchungsdatum im Beleg     Posting date in the document
  g_s_fieldcat-fieldname     = 'BUDAT'.
  g_s_fieldcat-ref_tabname   = 'MKPF'.
  g_s_fieldcat-sp_group      = 'D'.
  perform  f0410_fieldcat    using  c_take   c_out.

  g_s_fieldcat-fieldname     = 'MENGE'.     " Menge
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Quantity
  g_s_fieldcat-qfieldname    = 'MEINS'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_out.

  g_s_fieldcat-fieldname     = 'MEINS'.     " Basismengeneinheit
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Base unit of measure
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_out.

  if not bwbst is initial.   "mit bewertetem Bestand
*   Betrag in Hauswaehrung   Amount in local currency
    g_s_fieldcat-fieldname     = 'DMBTR'.
    g_s_fieldcat-ref_tabname   = 'BSIM'.
    g_s_fieldcat-cfieldname    = 'WAERS'.
    g_s_fieldcat-sp_group      = 'M'.
    perform  f0410_fieldcat    using  c_take   c_out.
  endif.

  if not bwbst is initial.   "mit bewertetem Bestand
    g_s_fieldcat-fieldname     = 'KSL'.
    g_s_fieldcat-ref_tabname   = 'FAGLFLEXA'.
    g_s_fieldcat-cfieldname    = 'WAERS'.
    g_s_fieldcat-sp_group      = 'M'.
    perform  f0410_fieldcat    using  c_take   c_out.
  endif.
  "note 201670
*check if segmentation switch is active
  if cl_ops_switch_check=>sfsw_segmentation_02( ) eq abap_on.
    g_s_fieldcat-fieldname     = 'SGT_SCAT'.     " Basismengeneinheit
    g_s_fieldcat-ref_tabname   = 'MSEG'.      " Base unit of measure
    g_s_fieldcat-sp_group      = 'M'.
    perform  f0410_fieldcat    using  c_take   c_out.
  endif.
* g_s_fieldcat-fieldname     = 'WAERS'.     " Waehrungs-schluessel
* g_s_fieldcat-ref_tabname   = 'T001'.      " Currency Key
* g_s_fieldcat-sp_group      = 'M'.
* perform  f0410_fieldcat    using  c_take   c_out.

* the following fields are always in g_s_mseg_lean, but they are
* hidden in the list
  g_s_fieldcat-fieldname     = 'MJAHR'.     " Materialbelegjahr
  g_s_fieldcat-ref_tabname   = 'MKPF'.      " Material doc. year
  g_s_fieldcat-sp_group      = 'D'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'GJAHR'.     " Geschäftsjahr
  g_s_fieldcat-ref_tabname   = 'BKPF'.      " Fiscal Year
  g_s_fieldcat-sp_group      = 'D'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'VGART'.    " Vorgangsart
  g_s_fieldcat-ref_tabname   = 'MKPF'.     " Transaction/event type
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'USNAM'.    " Name des Benutzers
  g_s_fieldcat-ref_tabname   = 'MKPF'.     " User name
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'CPUDT'.    " Tag der Erfassung
  g_s_fieldcat-ref_tabname   = 'MKPF'.     " Acc. doc. entry date
  g_s_fieldcat-sp_group      = 'D'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'CPUTM'.     " Uhrzeit der Erfassung
  g_s_fieldcat-ref_tabname   = 'MKPF'.      " Time of entry
  g_s_fieldcat-sp_group      = 'D'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'SHKZG'.    " Soll-/Haben-Kennzeichen
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " Debit/credit indicator
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'BWTAR'.     " Bewertungsart
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Valuation type
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

* Kennzeichen Bewertung Sonderbestand
* Indicator: valuation of special stock
  g_s_fieldcat-fieldname     = 'KZBWS'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'CHARG'.     " Chargennummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Batch number
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'BUKRS'.     " Buchungskreis
  g_s_fieldcat-ref_tabname   = 'T001'.      " Company code
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  if gv_switch_ehp6ru = abap_true and bwbst = 'X'.
*   G/L account
    g_s_fieldcat-fieldname     = 'HKONT'.
    g_s_fieldcat-ref_tabname   = 'BSEG'.
    g_s_fieldcat-sp_group      = 'O'.
    perform  f0410_fieldcat    using  c_take   c_no_out.
  endif.

  g_s_fieldcat-fieldname     = 'KZBEW'.     " Bewegungskennzeichen
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Movement indicator
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'KZVBR'.     " Kennz. Verbrauchsbuchung
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Consumption posting
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'KZZUG'.     " Zugangskennzeichen
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Receipt indicator
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'BUSTM'. " Buchungsstring für Mengen
  g_s_fieldcat-ref_tabname   = 'MSEG'.  " Posting string for quantities
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

  g_s_fieldcat-fieldname     = 'BUSTW'.    " Buchungsstring für Werte
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " Posting string for values
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

* Kennzeichen: Mengenfortschreibung im Materialstammsatz
* Quantity Updating in Material Master Record
  g_s_fieldcat-fieldname     = 'MENGU'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

* Kennzeichen: Wertfortschreibung im Materialstammsatz
* Value Updating in Material Master Record
  g_s_fieldcat-fieldname     = 'WERTU'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

* Bewegungsartengruppe zur Bestandsauswertung
* Movement type group for stock analysis
  g_s_fieldcat-fieldname     = 'BWAGR'.

* the reference table changed in release 46B
  g_s_fieldcat-ref_tabname   = 'T156Q'.

  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_take   c_no_out.

* process 'goods receipt/issue slip' as hidden field        "n450596
  g_s_fieldcat-fieldname     = 'XABLN'.                     "n450596
  g_s_fieldcat-ref_tabname   = 'MKPF'.                      "n450596
  g_s_fieldcat-sp_group      = 'S'.                         "n450596
  perform  f0410_fieldcat    using  c_take   c_no_out.      "n450596

* the following fields will be processed if they are in working table
* g_t_mseg_fields         Customer Exit :
* these fields can be activated in include RM07MLBD_CUST_FIELDS

  g_s_fieldcat-fieldname     = 'INSMK'.    " Bestandsart
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " stock type
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'LIFNR'.    " Kontonummer Lieferant
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " vendor's account number
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'KUNNR'.    " Kontonummer des Kunden
  g_s_fieldcat-ref_tabname   = 'MSEG'.   " account number of customer
  g_s_fieldcat-sp_group      = 'V'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* process the sales order number and sales order item  "n599218
* depending on the release                             "n599218
* release          field                               "n599218
* 4.5B and higher  MAT_KDAUF, MAT_KDPOS                "n599218
* 4.0B             KDAUF,     KDPOS                    "n599218
*                                                      "n599218
  g_s_fieldcat-fieldname   = 'MAT_KDAUF'.                   "n599218
  g_s_fieldcat-ref_tabname = 'MSEG'.                        "n599218
  g_s_fieldcat-sp_group    = 'V'.                           "n599218
  perform  f0410_fieldcat  using  c_check  c_no_out.        "n599218
                                                            "n599218
  g_s_fieldcat-fieldname   = 'MAT_KDPOS'.                   "n599218
  g_s_fieldcat-ref_tabname = 'MSEG'.                        "n599218
  g_s_fieldcat-sp_group    = 'V'.                           "n599218
  perform  f0410_fieldcat  using  c_check  c_no_out.        "n599218

  g_s_fieldcat-fieldname     = 'KDAUF'.     " Kundenauftragsnummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Sales Order Number
  g_s_fieldcat-sp_group      = 'V'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'KDPOS'.    " Positionsnummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " Item number in Sales Order
  g_s_fieldcat-sp_group      = 'V'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Einteilung Kundenauftrag   Delivery schedule for sales order
  g_s_fieldcat-fieldname     = 'KDEIN'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'F'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Menge in Erfassungsmengeneinheit   Quantity in unit of entry
  g_s_fieldcat-fieldname     = 'ERFMG'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-qfieldname    = 'ERFME'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'ERFME'.     " Erfassungsmengeneinheit
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Unit of entry
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Menge in Bestellpreismengeneinheit
* Quantity in purchase order price unit
  g_s_fieldcat-fieldname     = 'BPMNG'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-qfieldname    = 'BPRME'.
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'BPRME'.     " Bestellpreismengeneinheit
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Order price unit
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'EBELN'.     " Bestellnummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Purchase order number
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Positionsnummer des Einkaufsbelegs
* Item Number of Purchasing Document
  g_s_fieldcat-fieldname     = 'EBELP'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'SJAHR'.     " Materialbelegjahr
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Material doc. year
  g_s_fieldcat-sp_group      = 'D'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'SMBLN'.     " Nummer des Materialbelegs
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Number of material doc.
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'SMBLP'.     " Position im Materialbeleg
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Item in material document
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'ELIKZ'.  " Endlieferungskennzeichen
  g_s_fieldcat-ref_tabname   = 'MSEG'.   "Delivery completed" indicator
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'SGTXT'.     " Positionstext
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Item Text
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'WEMPF'.     " Warenempfänger
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Goods recipient
  g_s_fieldcat-sp_group      = 'V'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'ABLAD'.     " Abladestelle
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Unloading point
  g_s_fieldcat-sp_group      = 'V'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'GSBER'.     " Geschäftsbereich
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Business Area
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Geschäftsbereich des Geschäftspartners
* Trading partner's business area
  g_s_fieldcat-fieldname     = 'PARGB'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'PARBU'.   " Verrechnender Buchungskreis
  g_s_fieldcat-ref_tabname   = 'MSEG'.    " Clearing company code
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'KOSTL'.     " Kostenstelle
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Cost Center
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'AUFNR'.     " Auftragsnummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Order Number
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'ANLN1'.     " Anlagen-Hauptnummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Main asset number
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Nummer der Reservierung / des Sekundärbedarfs
* Number of reservation/dependent requirements
  g_s_fieldcat-fieldname     = 'RSNUM'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Positionsnummer der Reservierung / des Sekundärbedarfs
* Item number of reservation/dependent requirements
  g_s_fieldcat-fieldname     = 'RSPOS'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Final issue for this reservation
  g_s_fieldcat-fieldname     = 'KZEAR'.    " Kennzeichen: Endausfassung
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* empfangendes/abgebendes Material
* Receiving/issuing material
  g_s_fieldcat-fieldname     = 'UMMAT'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Empfangendes/Abgebendes Werk
* Receiving plant/issuing plant
  g_s_fieldcat-fieldname     = 'UMWRK'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Empfangender/Abgebender Lagerort
* Receiving/issuing storage location
  g_s_fieldcat-fieldname     = 'UMLGO'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'UMCHA'.  " Empfangende/Abgebende Charge
  g_s_fieldcat-ref_tabname   = 'MSEG'.   " Receiving/issuing batch
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Empfangende/Abgebende Bwertungsart
* Valuation type of transfer batch
  g_s_fieldcat-fieldname     = 'UMBAR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Sonderbestandskennzeichen der Umlagerung
* Special stock indicator for physical stock transfer
  g_s_fieldcat-fieldname     = 'UMSOK'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Kennzeichen Wareneingang unbewertet
* Goods receipt, non-valuated
  g_s_fieldcat-fieldname     = 'WEUNB'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Kennzeichen: Grund der Bewegung
* Reason for movement
  g_s_fieldcat-fieldname     = 'GRUND'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'KSTRG'.    " Kostenträger
  g_s_fieldcat-ref_tabname   = 'MSEG'.  " Cost Object
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Nummer für Ergebnisobjekte (CO-PA)
* Profitability segment number (CO-PA)
  g_s_fieldcat-fieldname     = 'PAOBJNR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'PRCTR'.     " Profit Center
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Profit Center
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Projektstrukturplanelement (PSP-Element)
* Work breakdown structure element (WBS element)

* process the WBS element depends on the release       "n599218
* release          field                               "n599218
* 4.5B and higher  MAT_PSPNR                           "n599218
* 4.0B             PS_PSP_PNR                          "n599218
*                                                      "n599218
  g_s_fieldcat-fieldname   = 'MAT_PSPNR'.                   "n599218
  g_s_fieldcat-ref_tabname = 'MSEG'.                        "n599218
  g_s_fieldcat-sp_group    = 'K'.                           "n599218
  perform  f0410_fieldcat  using  c_check  c_no_out.        "n599218

  g_s_fieldcat-fieldname     = 'PS_PSP_PNR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Netzplannummer für Kontierung
* Network Number for Account Assignment
  g_s_fieldcat-fieldname     = 'NPLNR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Plannummer zu Vorgängen im Auftrag
* Routing number for operations in the order
  g_s_fieldcat-fieldname     = 'AUFPL'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'AUFPS'.   " Nummer der Auftragsposition
  g_s_fieldcat-ref_tabname   = 'MSEG'.    " Order item number
  g_s_fieldcat-sp_group      = 'K'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Wareneingangsmenge in Bestellmengeneinheit
* Goods receipt quantity in order unit
  g_s_fieldcat-fieldname     = 'BSTMG'.
  g_s_fieldcat-qfieldname    = 'BSTME'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'BSTME'.    " Bestellmengeneinheit
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " Order unit
  g_s_fieldcat-sp_group      = 'E'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Extern eingegebener Buchungsbetrag in Hauswährung
* Externally entered posting amount in local currency
  g_s_fieldcat-fieldname     = 'EXBWR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-cfieldname    = 'WAERS'.
  g_s_fieldcat-sp_group      = 'S'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Wert zu Verkaufspreisen mit Mehrwertsteuer
* Value at sales prices including value-added tax
  g_s_fieldcat-fieldname     = 'VKWRT'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-cfieldname    = 'WAERS'.
  g_s_fieldcat-sp_group      = 'V'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Verfallsdatum oder Mindesthaltbarkeitsdatum
* Shelf Life Expiration Date
  g_s_fieldcat-fieldname     = 'VFDAT'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Extern eingegebener Verkaufswert in Hauswährung
* Externally entered sales value in local currency
  g_s_fieldcat-fieldname     = 'EXVKW'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-cfieldname    = 'WAERS'.
  g_s_fieldcat-sp_group      = 'S'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'PPRCTR'.    " Partner-Profit Center
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Partner-Profit Center
  g_s_fieldcat-sp_group      = 'O'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Material, auf dem der Bestand geführt wird
* Material on which stock is managed
  g_s_fieldcat-fieldname     = 'MATBF'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Empfangendes/Abgebendes Material
* Receiving/issuing material
  g_s_fieldcat-fieldname     = 'UMMAB'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Gesamter bewerteter Bestand
* Total valuated stock before the posting
  g_s_fieldcat-fieldname     = 'LBKUM'.
  g_s_fieldcat-qfieldname    = 'MEINS'.                    "note 201670
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Wert des gesamten bewerteten Bestandes
* Value of total valuated stock before the posting
  g_s_fieldcat-fieldname     = 'SALK3'.
  g_s_fieldcat-cfieldname    = 'WAERS'.                    "note 201670
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'VPRSV'.    " Preissteuerungskennzeichen
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " Price control indicator
  g_s_fieldcat-sp_group      = 'S'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Wert zu Verkaufspreisen ohne Mehrwertsteuer
* Value at sales prices excluding value-added tax
  g_s_fieldcat-fieldname     = 'VKWRA'.
  g_s_fieldcat-cfieldname    = 'WAERS'.                   "note 201670
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'S'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Ursprungszeile im Materialbeleg
* Original line in material document
  g_s_fieldcat-fieldname     = 'URZEI'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'S'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Menge in Mengeneinheit aus Lieferschein
* Quantity in unit of measure from delivery note
  g_s_fieldcat-fieldname     = 'LSMNG'.
  g_s_fieldcat-qfieldname    = 'LSMEH'.                  "note 201670
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* Mengeneinheit aus Lieferschein
* Unit of measure from delivery note
  g_s_fieldcat-fieldname     = 'LSMEH'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  perform  f0410_fieldcat    using  c_check  c_no_out.

* if the field catalog contains a field with values in currency,
* add the currency to to field-catalogue
  data : l_cnt_waers_active type i,                         "n497992
         l_cnt_waers_total  type i.                         "n497992
                                                            "n497992
  loop at fieldcat           into  g_s_fieldcat.            "n497992
    check : g_s_fieldcat-cfieldname    = 'WAERS'.           "n497992
*   this field has a reference to the currency key          "n497992
    add  1                   to  l_cnt_waers_total.         "n497992
                                                            "n497992
    check : g_s_fieldcat-no_out is initial.                 "n497992
*   this field is active                                    "n497992
    add  1                   to  l_cnt_waers_active.        "n497992
  endloop.                                                  "n497992
                                                            "n497992
  if    l_cnt_waers_active > 0.                             "n497992
*   there is at least one active reference field            "n497992
*   declare currency key WAERS active, too                  "n497992
    g_s_fieldcat-fieldname     = 'WAERS'.   "Currency Key   "n497992
    g_s_fieldcat-ref_tabname   = 'T001'.                    "n497992
    g_s_fieldcat-sp_group      = 'M'.                       "n497992
    perform  f0410_fieldcat    using  c_take   c_out.       "n497992
                                                            "n497992
  elseif  l_cnt_waers_total > 0.                            "n497992
*   there are only hidden reference fields                  "n497992
*   declare currency key WAERS hidden, too                  "n497992
    g_s_fieldcat-fieldname     = 'WAERS'.   "Currency Key   "n497992
    g_s_fieldcat-ref_tabname   = 'T001'.                    "n497992
    g_s_fieldcat-sp_group      = 'M'.                       "n497992
    perform  f0410_fieldcat    using  c_take   c_no_out.    "n497992
  endif.                                                    "n497992
*  ENHANCEMENT-POINT rm07mlbd_04 SPOTS es_rm07mlbd.
  if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_15\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
*
* Only if no valuated Stock should display
    if bwbst <> 'X'.
      clear: g_s_fieldcat-no_out.
      g_s_fieldcat-fieldname     = '/CWM/MENGE'.        " Menge
      g_s_fieldcat-ref_tabname   = 'MSEG'.              " Quantity
      g_s_fieldcat-qfieldname    = '/CWM/MEINS'.
      g_s_fieldcat-sp_group      = 'M'.
      perform  f0410_fieldcat    using  c_take   c_out.
    endif.

* Only if no valuated Stock should display
    if bwbst <> 'X'.
      clear: g_s_fieldcat-no_out.
      g_s_fieldcat-fieldname     = '/CWM/MEINS'.        " Basismengeneinheit
      g_s_fieldcat-ref_tabname   = 'MSEG'.              " Base unit of measure
      g_s_fieldcat-sp_group      = 'M'.
      perform  f0410_fieldcat    using  c_take   c_out.
    endif.

* Now set the minimum Line-Size for new fields in Header, so
* all values in Headerline (now length = 84) will be shown and
* remain intact even the user reduce the number of visible fields
    layout-min_linesize = 85.

* use ref to /CWM/VALUM
    if bwbst = 'X'.
      loop at fieldcat           into  g_s_fieldcat.
        check : g_s_fieldcat-fieldname    = 'MEINS'.
        g_s_fieldcat-ref_tabname = 'MARA'.
        g_s_fieldcat-ref_fieldname = '/CWM/VALUM'.
        modify fieldcat from g_s_fieldcat.
      endloop.
    endif.
  endif.
*  ENHANCEMENT-POINT ehp605_rm07mlbd_15 SPOTS es_rm07mlbd .

endform.                     "f0400_create_fieldcat

*----------------------------------------------------------------------*
*    F0410_FIELDCAT
*----------------------------------------------------------------------*

form f0410_fieldcat
         using  l_f_check
                l_f_no_out   type      slis_fieldcat_main-no_out.

  data : l_f_continue(01) type c,
         l_f_type(01)     type c,
         l_f_fieldname    type      stype_fields.

  field-symbols : <l_fs>.

  if  l_f_check = c_take.
*   take this entry without check
    move  'X'                to  l_f_continue.
  else.
*   create key and look for fieldname
    concatenate              g_s_fieldcat-ref_tabname
                             '~'
                             g_s_fieldcat-fieldname
                             into l_f_fieldname.

    read table g_t_mseg_fields         into g_s_mseg_fields
                             with key
                             fieldname = l_f_fieldname
                             binary search.

    if sy-subrc is initial.
      move  'X'              to  l_f_continue.
    else.
*     additional fields are displayed in wrong format :     "n480130
*     clear the working area for the field catalog when     "n480130
*     the current field should not be processed             "n480130
      clear                  g_s_fieldcat.                  "n480130
      clear                  l_f_continue.
    endif.
  endif.

* append entry to field catalog if field is in structure
* else leave this routine
  if l_f_continue is initial.
    clear                    g_s_fieldcat.
    exit.
  endif.

  if  not l_f_no_out is initial.
    move  l_f_no_out         to  g_s_fieldcat-no_out.
  endif.

  add  : 1                   to  g_f_col_pos.
  move : g_f_col_pos         to  g_s_fieldcat-col_pos,
         g_f_tabname         to  g_s_fieldcat-tabname.
  append g_s_fieldcat        to  fieldcat.

* create the table with the fields who will be enriched with colors
* and sign
  if  g_s_fieldcat-fieldname  =  'MENGE'  or
      g_s_fieldcat-fieldname  =  'MEINS'  or
      g_s_fieldcat-fieldname  =  'DMBTR'  or
      g_s_fieldcat-fieldname  =  'WAERS'  or
      g_s_fieldcat-fieldname  =  'ERFMG'  or
      g_s_fieldcat-fieldname  =  'ERFME'  or

      g_s_fieldcat-fieldname  =  'BPMNG'  or
      g_s_fieldcat-fieldname  =  'BPRME'  or
      g_s_fieldcat-fieldname  =  'BSTMG'  or
      g_s_fieldcat-fieldname  =  'BSTME'  or
      g_s_fieldcat-fieldname  =  'EXBWR'  or
      g_s_fieldcat-fieldname  =  'VKWRT'  or

      g_s_fieldcat-fieldname  =  'EXVKW'  or
      g_s_fieldcat-fieldname  =  'VKWRA'  or
      g_s_fieldcat-fieldname  =  'LSMNG'  or
      g_s_fieldcat-fieldname  =  'LSMEH'  or
      g_s_fieldcat-fieldname  =  'SHKZG'.

*   look for the type of this field
    concatenate              g_s_fieldcat-ref_tabname
                             '-'
                             g_s_fieldcat-fieldname
                             into l_f_fieldname.

    assign  (l_f_fieldname)  to <l_fs>.

    if  sy-subrc is initial.
      describe field <l_fs>    type  l_f_type.
      move : g_s_fieldcat-fieldname
                             to  g_t_color_fields-fieldname,
           l_f_type          to  g_t_color_fields-type.
      append                 g_t_color_fields.
    endif.
  endif.
  log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_16\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
*
  if  g_s_fieldcat-fieldname  =  '/CWM/MENGE'  or
      g_s_fieldcat-fieldname  =  '/CWM/MEINS'  or
      g_s_fieldcat-fieldname  =  '/CWM/ERFMG'  or
      g_s_fieldcat-fieldname  =  '/CWM/ERFME'.
*   look for the type of this field
    concatenate              g_s_fieldcat-ref_tabname
                             '-'
                             g_s_fieldcat-fieldname
                             into l_f_fieldname.

    assign  (l_f_fieldname)  to <l_fs>.

    if  sy-subrc is initial.
      describe field <l_fs>    type  l_f_type.
      move : g_s_fieldcat-fieldname
                             to  g_t_color_fields-fieldname,
           l_f_type          to  g_t_color_fields-type.
      append                 g_t_color_fields.
    endif.
  endif.

*  ENHANCEMENT-POINT ehp605_rm07mlbd_16 SPOTS es_rm07mlbd .

  clear                      g_s_fieldcat.

endform.                     "F0410_FIELDCAT

*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form belege_ergaenzen_2.                                    "n443935

  data: lv_bsim_key(5) type c.                              "2936002

* control break : process the entries from a group          "n443935
                                                            "n443935
* look for the matching FI documents with set and get       "n443935
  move : g_s_mseg_old-mblnr  to  matkey-mblnr,              "n443935
         g_s_mseg_old-mjahr  to  matkey-mjahr.              "n443935
                                                            "n443935
  lv_bsim_key = 'BSIM'.                                     "2936002
  read  table g_t_bsim_lean  into  g_s_bsim_lean            "n443935
                   with table key bsim components           "2936002
                             bukrs  =  g_s_mseg_old-bukrs   "n443935
                             bwkey  =  g_s_mseg_old-bwkey   "n443935
                             matnr  =  g_s_mseg_old-matnr   "n443935
                             bwtar  =  g_s_mseg_old-bwtar   "n443935
                             shkzg  =  g_s_mseg_old-shkzg   "n443935
                             meins  =  g_s_mseg_old-meins   "n443935
                             budat  =  g_s_mseg_old-budat   "n443935
                             blart  =  g_s_mseg_old-blart   "n443935
                             awkey  =  matkey.              "n443935
  if sy-subrc is not initial.                               "2575057
    lv_bsim_key = 'FUZZY'.                                  "2936002
    read  table g_t_bsim_lean  into  g_s_bsim_lean          "2575057
                 with table key fuzzy components            "2936002
                           bukrs  =  g_s_mseg_old-bukrs     "2575057
                           bwkey  =  g_s_mseg_old-bwkey     "2575057
                           matnr  =  g_s_mseg_old-matnr     "2575057
                           bwtar  =  g_s_mseg_old-bwtar     "2575057
                           shkzg  =  g_s_mseg_old-shkzg     "2575057
*                            meins  =  g_s_mseg_old-meins      "2722841
                           budat  =  g_s_mseg_old-budat     "2575057
                           awkey  =  matkey.                "2575057
  endif.                                                    "2575057
  if sy-subrc is initial.                                   "n443935
    move  sy-tabix           to  g_f_tabix_start.           "n443935
                                                            "n443935
*   continue with sequential read of working table          "n443935
    loop at g_t_bsim_lean    into  g_s_bsim_lean            "n443935
                             from  g_f_tabix_start          "n443935
                             using key (lv_bsim_key). "#EC CI_NOORDER "2936002
                                                            "n443935
      if  g_s_bsim_lean-bukrs  =  g_s_mseg_old-bukrs  and   "n443935
          g_s_bsim_lean-bwkey  =  g_s_mseg_old-bwkey  and   "n443935
          g_s_bsim_lean-matnr  =  g_s_mseg_old-matnr  and   "n443935
          g_s_bsim_lean-bwtar  =  g_s_mseg_old-bwtar  and   "n443935
          g_s_bsim_lean-shkzg  =  g_s_mseg_old-shkzg  and   "n443935
*         g_s_bsim_lean-meins  =  g_s_mseg_old-meins  AND      "2722841
          g_s_bsim_lean-budat  =  g_s_mseg_old-budat  and   "n443935
*         IS-OIL posts GR for PO with "MB11" which leads to    "2575057
*         WA in MKPF and WE in BSIM. This is against the       "2575057
*         core rule. Implement a light fuzzy search, check     "2575057
*         only for BLART starting with W like WE,WA,WF,WI,WL.  "2575057
          g_s_bsim_lean-blart(1) = g_s_mseg_old-blart(1) and "2707850
          g_s_bsim_lean-awkey  =  matkey.                   "n443935
*       select all matching entries                         "n443935
        add   1              to  g_cnt_bsim_entries.        "n443935
        move-corresponding  g_s_bsim_lean                   "n443935
                             to  g_s_bsim_work.             "n443935
        move  sy-tabix       to  g_s_bsim_work-tabix.       "n443935
        append g_s_bsim_work to  g_t_bsim_work.             "n443935
      else.                                                 "n443935
        if g_s_bsim_lean-awkey <> matkey.                   "2575057
          exit.                                             "n443935
        endif.                                              "2575057
      endif.                                                "n443935
    endloop.                                                "n443935
  endif.                                                    "n443935

  if  g_flag_break-b1 = 'X'.                                "n921164
    break-point                id mmim_rep_mb5b.            "n921164
*   dynamic break-point : results in contol break           "n921164
  endif.

* how many matching entries from BSIM found ?               "n443935
  if      g_cnt_bsim_entries is initial.                    "n443935
*   no BSIM entries found -> no action.                     "n443935
                                                            "n443935
  elseif  g_cnt_bsim_entries = 1  and                       "n443935
          g_cnt_mseg_entries = 1.                           "n443935
*   the ideal case 1 MM and 1 FI document;                  "n443935
*   mark this FI doc for deletion                           "n443935
    loop at g_t_bsim_work    into  g_s_bsim_work.           "n443935
      read  table  g_t_bsim_lean  into  g_s_bsim_lean       "n443935
                             index  g_s_bsim_work-tabix     "n443935
                             using key (lv_bsim_key).       "2936002
                                                            "n443935
      check : sy-subrc is initial.                          "n443935
      move  : 'D'            to  g_s_bsim_lean-accessed.    "n443935
      modify  g_t_bsim_lean  from  g_s_bsim_lean            "n443935
                             index  g_s_bsim_work-tabix     "n443935
                             using key (lv_bsim_key)        "2936002
                             transporting  accessed.        "n451923
                                                            "n443935
*     set the FI doc number into the entry of the MM doc    "n443935
      read  table  g_t_mseg_work  into  g_s_mseg_work       "n443935
                             index  1.                      "n443935
      check : sy-subrc is initial.                          "n443935
                                                            "n443935
      move : g_s_bsim_work-belnr                            "n443935
                             to  g_s_mseg_work-belnr,       "n443935
             g_s_bsim_work-gjahr                            "n443935
                             to  g_s_mseg_work-gjahr.       "n443935
      if gv_switch_ehp6ru = abap_true.
        move: g_s_bsim_work-buzei
                             to  g_s_mseg_work-buzei.
        move-corresponding g_s_bsim_work to g_t_bseg_key.
        append g_t_bseg_key.
      endif.

*     consider special gain/loss-handling of IS-OIL         "n497992
**# IF EXIST OI001
**"    if  g_s_mseg_work-oiglcalc = 'L'  and            "n497992
**"        g_s_mseg_work-shkzg    = 'H'  and            "n497992
**"        g_s_mseg_work-dmbtr    = 0.                  "n497992
**"      move  g_s_bsim_work-dmbtr                      "n497992
**"                  to  g_s_mseg_work-dmbtr.           "n497992
**"    endif.                                           "n497992
**"                                                     "n497992
**"    MODIFY G_T_MSEG_work                             "n497992
**"                 FROM  G_S_MSEG_work                 "n497992
**"                 INDEX  1                            "n497992
**"                 TRANSPORTING BELNR GJAHR dmbtr.     "n497992
**# ELSE
*      MODIFY G_T_MSEG_work  FROM  G_S_MSEG_work        "n443935
*                            INDEX  1                   "n443935
*                            TRANSPORTING  BELNR GJAHR. "n443935
**# ENDIF
*     IS-OIL specific functions without ABAP preprocessor   "n599218 A
      if  g_flag_is_oil_active = 'X'.       "IS-OIL ?       "n599218 A
        if  g_s_mseg_work-oiglcalc = 'L'  and               "n599218 A
            g_s_mseg_work-shkzg    = 'H'  and               "n599218 A
            g_s_mseg_work-dmbtr    = 0.                     "n599218 A
          move  g_s_bsim_work-dmbtr                         "n599218 A
                             to  g_s_mseg_work-dmbtr.       "n599218 A
        endif.                                              "n599218 A
                                                            "n599218 A
        modify g_t_mseg_work                                "n599218 A
                   from  g_s_mseg_work                      "n599218 A
                   index  1                                 "n599218 A
                   transporting belnr gjahr buzei dmbtr.
      else.                                                 "n599218 A
        modify g_t_mseg_work  from  g_s_mseg_work           "n599218 A
                            index  1                        "n599218 A
                            transporting  belnr gjahr buzei.
      endif.                                                "n599218 A

    endloop.                                                "n443935
                                                            "n443935
  else.                                                     "n443935
*   there are a lot of MM docs                              "n443935
    perform                  belege_ergaenzen_several_docs
                                         using lv_bsim_key. "2963312
                                                            "n443935
  endif.                                                    "n443935
                                                            "n443935
* copy the number and fiscal year into the matching         "n451923
* entry of the main table G_T_MSEG_LEAN                     "n451923
  loop at g_t_mseg_work      into  g_s_mseg_work.           "n451923
*   only with useful FI doc data                            "n451923
    check : not g_s_mseg_work-belnr is initial.             "n451923
                                                            "n443935
*   read the original entry and change it                   "n451923
    read table g_t_mseg_lean into  g_s_mseg_update          "n451923
                             index g_s_mseg_work-tabix.     "n451923
                                                            "n443935
    check : sy-subrc is initial.   "entry found ?           "n451923
    move  : g_s_mseg_work-belnr                             "n451923
                             to  g_s_mseg_update-belnr,     "n451923
            g_s_mseg_work-gjahr                             "n451923
                             to  g_s_mseg_update-gjahr.     "n451923
    if gv_switch_ehp6ru = abap_true.
      move: g_s_mseg_work-buzei
                             to  g_s_mseg_update-buzei.
      move-corresponding g_s_mseg_work to g_t_bseg_key.
      append g_t_bseg_key.
    endif.

*   consider special gain/loss-handling of IS-OIL           "n497992
**# IF EXIST OI001
**"  move  g_s_mseg_work-dmbtr                          "n497992
**"                 to  g_s_mseg_update-dmbtr.          "n497992
**"                                                     "n497992
**"  MODIFY G_T_MSEG_lean                               "n497992
**"                 FROM  G_S_MSEG_update               "n497992
**"                 index g_s_mseg_work-tabix           "n497992
**"                 TRANSPORTING BELNR GJAHR dmbtr.     "n497992
**# ELSE
*    modify  g_t_mseg_lean  from  g_s_mseg_update       "n451923
*                           index g_s_mseg_work-tabix   "n451923
*                           transporting  belnr gjahr.  "n451923
**# ENDIF
*   IS-OIL specific functions without ABAP preprocessor     "n599218 A
    if  g_flag_is_oil_active = 'X'.        "IS-OIL ?       "n599218 A
      move  g_s_mseg_work-dmbtr                             "n599218 A
                             to  g_s_mseg_update-dmbtr.     "n599218 A
                                                            "n599218 A
      modify g_t_mseg_lean                                  "n599218 A
                 from  g_s_mseg_update                      "n599218 A
                 index g_s_mseg_work-tabix                  "n599218 A
                 transporting belnr gjahr buzei dmbtr.
    else.                                                   "n599218 A
      modify g_t_mseg_lean from  g_s_mseg_update            "n599218 A
                           index g_s_mseg_work-tabix        "n599218 A
                           transporting  belnr gjahr buzei.
    endif.                                                  "n599218 A

  endloop.                                                  "n451923

  perform                    belege_ergaenzen_clear.        "n443935
                                                            "n443935
endform.                     "belege_ergaenzen_2            "n443935
                                                            "n443935
*&----------------------------------------------------------"n443935
*& belege_ergaenzen_clear
*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form belege_ergaenzen_clear.                                "n443935
                                                            "n443935
* clear working areas for the next group                    "n443935
  refresh : g_t_bsim_work,   g_t_mseg_work.                 "n443935
  clear   : g_cnt_mseg_entries, g_cnt_mseg_done,            "n443935
            g_cnt_bsim_entries.                             "n443935
                                                            "n443935
endform.                     "belege_ergaenzen_clear.       "n443935
                                                            "n443935
*&----------------------------------------------------------"n443935
*    belege_ergaenzen_several_docs
*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_several_docs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form belege_ergaenzen_several_docs using p_bsim_key type c. "2963312
*
  data lv_/cwm/menge        like g_s_mseg_work-menge.
*  ENHANCEMENT-POINT ehp605_rm07mlbd_17 SPOTS es_rm07mlbd STATIC .
                                                            "n443935
* first step : the entries must hit quantity and value      "n443935
  loop at g_t_mseg_work    into  g_s_mseg_work.             "n443935
                                                            "n443935
*   look for a matching FI doc                              "n443935
    loop at g_t_bsim_work  into  g_s_bsim_work.             "n443935
*     ingnore the entries for deletion                      "n443935
      check : g_s_bsim_work-accessed ne 'D'.                "n443935
                                                            "n443935
      if  g_s_bsim_work-menge = g_s_mseg_work-menge  and    "n443935
          g_s_bsim_work-dmbtr = g_s_mseg_work-dmbtr.        "n443935
*       mark the entries                                    "n443935
        add  1               to  g_cnt_mseg_done.           "n443935
        move : g_s_bsim_work-belnr                          "n443935
                             to  g_s_mseg_work-belnr,       "n443935
               g_s_bsim_work-gjahr                          "n443935
                             to  g_s_mseg_work-gjahr.       "n443935
        if gv_switch_ehp6ru = abap_true.
          move: g_s_bsim_work-buzei
                             to  g_s_mseg_work-buzei.
          move-corresponding g_s_bsim_work to g_t_bseg_key.
          append g_t_bseg_key.
        endif.

        modify g_t_mseg_work  from  g_s_mseg_work           "n443935
                             transporting  belnr gjahr buzei.
                                                            "n443935
*       mark the entries for deletion                       "n443935
        move    'D'          to  g_s_bsim_work-accessed.    "n443935
        modify  g_t_bsim_work  from  g_s_bsim_work          "n443935
                             transporting  accessed.        "n451923
        exit.                "Stop at the firts hit         "n443935
      endif.                                                "n443935
    endloop.                                                "n443935
  endloop.                                                  "n443935

  if  g_flag_break-b2 = 'X'.                                "n921164
    break-point                id mmim_rep_mb5b.            "n921164
*   dynamic break-point : in control break                  "n921164
  endif.                                                    "n921164

  if  g_cnt_mseg_entries  ne g_cnt_mseg_done.               "n443935
*   there are MM docs without FI doc left                   "n443935
                                                            "n443935
*     subtract the quantity and value from MM doc from      "n443935
*     the fields of the FI doc                              "n443935
    loop at g_t_mseg_work  into  g_s_mseg_work.             "n443935
                                                            "n443935
*       take only the entries without FI doc number         "n443935
      check : g_s_mseg_work-belnr is initial.               "n443935
                                                            "n443935
      loop at g_t_bsim_work  into  g_s_bsim_work.           "n443935
*         ingnore the entries for deletion                  "n443935
        check : g_s_bsim_work-accessed ne 'D'.              "n443935
                                                            "n443935
        if g_s_bsim_work-menge ge g_s_mseg_work-menge and   "n443935
           g_s_bsim_work-dmbtr ge g_s_mseg_work-dmbtr.      "n443935
                                                            "n443935
          if not g_s_mseg_work-dmbtr is initial.            "2117567
*           quantities without value are not in BSIM            "2117567
            subtract :                                      "n443935
              g_s_mseg_work-menge from  g_s_bsim_work-menge, "n443935
              g_s_mseg_work-dmbtr from  g_s_bsim_work-dmbtr. "n443935
          endif.                                            "2117567
                                                            "n443935
          if  g_s_bsim_work-menge  is initial  and          "n443935
              g_s_bsim_work-dmbtr  is initial.              "n443935
*           mark the entry for deletion                     "n443935
            move    'D'      to  g_s_bsim_work-accessed.    "n443935
          else.                                             "n443935
*           set the flag for check the merge process        "n443935
            move    'X'      to  g_s_bsim_work-accessed.    "n443935
          endif.                                            "n443935
                                                            "n443935
          modify  g_t_bsim_work  from  g_s_bsim_work        "n443935
*           change quantity and value in working table, too  "n747306
            transporting  accessed menge dmbtr.             "n747306
                                                            "n443935
*         mark the entries                                  "n443935
          add  1             to  g_cnt_mseg_done.           "n443935
          move : g_s_bsim_work-belnr                        "n443935
                             to  g_s_mseg_work-belnr,       "n443935
                 g_s_bsim_work-gjahr                        "n443935
                             to  g_s_mseg_work-gjahr.       "n443935
          if gv_switch_ehp6ru = abap_true.
            move: g_s_bsim_work-buzei
                             to  g_s_mseg_work-buzei.
            move-corresponding g_s_bsim_work to g_t_bseg_key.
            append g_t_bseg_key.
          endif.

          modify g_t_mseg_work  from  g_s_mseg_work         "n443935
                             transporting  belnr gjahr buzei.
          exit.              "Stop at the first hit         "n443935
        endif.                                              "n443935
      endloop.                                              "n443935
    endloop.                                                "n443935
  endif.                                                    "n443935
                                                            "n443935
* mark the processed FI docs for deletion                   "n443935
  loop at g_t_bsim_work    into  g_s_bsim_work.             "n443935
    check   g_s_bsim_work-accessed = 'D'.                   "n443935
                                                            "n443935
    read  table  g_t_bsim_lean  into  g_s_bsim_lean         "n443935
                             index  g_s_bsim_work-tabix     "n443935
                             using key (p_bsim_key).        "2963312
                                                            "n443935
    check : sy-subrc is initial.                            "n443935
    move  : 'D'              to  g_s_bsim_lean-accessed.    "n443935
    modify  g_t_bsim_lean    from   g_s_bsim_lean           "n443935
                             index  g_s_bsim_work-tabix     "n443935
                             using key (p_bsim_key)         "2963312
                             transporting  accessed.        "n451923
  endloop.                                                  "n443935
  log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_18\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
*
  loop at g_t_mseg_work      into  g_s_mseg_work.

    if /cwm/cl_md_material=>is_cw_material( g_s_mseg_work-matnr ) = /cwm/cl_md_material=>true.

      if g_s_mseg_work-matnr   <> imara-matnr.
        read table imara with key matnr = g_s_mseg_work-matnr
                         binary search.
      endif.
      if imara-/cwm/valum = g_s_mseg_work-/cwm/meins.
        lv_/cwm/menge            =  g_s_mseg_work-/cwm/menge.
        g_s_mseg_work-/cwm/menge =  g_s_mseg_work-menge.
        g_s_mseg_work-menge      =  lv_/cwm/menge.
        modify g_t_mseg_work   from  g_s_mseg_work
                             transporting  menge /cwm/menge.
      endif.
    endif.
  endloop.
*  ENHANCEMENT-POINT ehp605_rm07mlbd_18 SPOTS es_rm07mlbd .
                                                            "n443935
endform.                     "belege_ergaenzen_several_docs "n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND                                             *
*&---------------------------------------------------------------------*

form user_command                                           "#EC CALLED
                   using     r_ucomm      like  sy-ucomm
                             rs_selfield  type  slis_selfield.

  types: begin of ty_s_sel,
           mblnr like  mseg-mblnr,
           mjahr like  mseg-mjahr,
           zeile like  mseg-zeile,
           bukrs like  mseg-bukrs,
           belnr like  mseg-belnr,
           gjahr like  mseg-gjahr,
         end of ty_s_sel,

         ty_t_sel type ty_s_sel occurs 0.

  data: l_value(10) type c,                                 "n1583816
        ls_sel      type ty_s_sel,
        lt_sel      type ty_t_sel,
        l_lines     like sy-tabix,
        ls_fc       type slis_fieldcat_alv,
        lt_fc       type slis_t_fieldcat_alv,
        ls_selfield type slis_selfield,
        l_fi_doc    type c  length 1.                       "n1511550

* Unfortunately the output list of this report consists
* of several ALVs, one started at the end-event of the other.
* This abstrucse programming style was chosen to create a list
* layout similar to the one in release 3.1. Now this causes a severe
* problem: When selecting a line, we do not know which ALV (and there-
* for which line in table IMSEG) has been selected. We can only use
* the value of the selected field to access the data-table.
* In case of ambiguities, a popup has to be transmitted where the
* user has to reselect the document he wants to see. This is
* difficult to understand, if you do not know the problems of
* programming ABAP.

  case r_ucomm.
    when '9PBP'.
*     Get line of IMSEG which "look" like the one selected
      l_value = rs_selfield-value.
      check not l_value is initial.                         "204872
      call function 'CONVERSION_EXIT_ALPHA_INPUT'              "n1604106
        exporting                                              "n1604106
          input  = l_value                                     "n1604106
        importing                                              "n1604106
          output = l_value.                                    "n1604106
      if rs_selfield-sel_tab_field = 'G_T_BELEGE-MBLNR' or
         rs_selfield-sel_tab_field = 'G_T_BELEGE1-MBLNR'.
        loop at g_t_mseg_lean          into  g_s_mseg_lean
                                       where mblnr = l_value.
          ls_sel-mblnr = g_s_mseg_lean-mblnr.
          ls_sel-mjahr = g_s_mseg_lean-mjahr.
          ls_sel-zeile = g_s_mseg_lean-zeile.
          collect ls_sel into lt_sel.
        endloop.

      elseif rs_selfield-sel_tab_field = 'G_T_BELEGE-BELNR' or
             rs_selfield-sel_tab_field = 'G_T_BELEGE1-BELNR'.
        l_fi_doc = 'X'.
        loop at g_t_mseg_lean          into  g_s_mseg_lean
                                       where belnr = l_value.
          ls_sel-belnr = g_s_mseg_lean-belnr.
          ls_sel-gjahr = g_s_mseg_lean-gjahr.
          ls_sel-bukrs = g_s_mseg_lean-bukrs.
          collect ls_sel into lt_sel.
        endloop.

      endif.
      sort lt_sel by mjahr mblnr zeile bukrs belnr gjahr.
*     Read first line. If L_LINES = 1, LS_SEL is filled properly.
      read table lt_sel into ls_sel index 1.
      describe table lt_sel lines l_lines.
*     If no line found, the cursor was not on a useful value.
      if l_lines = 0.
        message s270.
        exit.
      endif.
*     If more than one line found, it gets difficult. We send a popup
*     where the user may select a single line.
      if l_lines > 1.
*       Create fieldcatalog
        define fc_add.
          ls_fc-fieldname     = &1.
          ls_fc-ref_tabname   = &2.
          ls_fc-ref_fieldname = &3.
          APPEND ls_fc TO lt_fc.
        end-of-definition.
        clear ls_sel.
        if l_fi_doc is initial.
          fc_add 'MBLNR' 'MKPF' 'MBLNR'.
          fc_add 'MJAHR' 'MKPF' 'MJAHR'.
          fc_add 'ZEILE' 'MSEG' 'ZEILE'.
        else.
          fc_add 'BUKRS' 'BKPF' 'BUKRS'.
          fc_add 'BELNR' 'BKPF' 'BELNR'.
          fc_add 'GJAHR' 'BKPF' 'GJAHR'.
        endif.

        call function 'REUSE_ALV_POPUP_TO_SELECT'
          exporting
            i_zebra     = 'X'
            i_tabname   = 'LT_SEL'
            it_fieldcat = lt_fc
          importing
            es_selfield = ls_selfield
          tables
            t_outtab    = lt_sel.
*       Read table with the unique index.
        read table lt_sel into ls_sel index ls_selfield-tabindex.
        if sy-subrc <> 0.
          exit.
        endif.
      endif.

*     read and save the user parameters before calling
*     transaction MIGO or FB03
      perform                user_parameters_save.

*     Now call the corresponding application. LS_SEL is always filled
*     correctly.
      if l_fi_doc is initial.

*     call the display transcation MIGO for the MM document "TEST
        call function 'MIGO_DIALOG'                       "n547170
          exporting                                       "n547170
            i_action            = 'A04'                   "n547170
            i_refdoc            = 'R02'                   "n547170
            i_notree            = 'X'                     "n547170
            i_no_auth_check     = ' '                     "n547170
            i_deadend           = 'X'                     "n547170
            i_skip_first_screen = 'X'                     "n547170
            i_okcode            = 'OK_GO'                 "n547170
            i_mblnr             = ls_sel-mblnr            "n547170
            i_mjahr             = ls_sel-mjahr            "n547170
            i_zeile             = ls_sel-zeile.           "n547170
      else.
        set parameter id 'BLN' field ls_sel-belnr.
        set parameter id 'BUK' field ls_sel-bukrs.
        set parameter id 'GJR' field ls_sel-gjahr.
        call transaction 'FB03'               "#EC CI_CALLTA  "n1511550
                             and skip first screen.         "n1511550
      endif.

*     restore the former user parameters
      perform                user_parameters_restore.

  endcase.

endform.                               " USER_COMMAND

*-----------------------------------------------------------"n547170
*    esdus_get_parameters                                   "n547170
*-----------------------------------------------------------"n547170

form esdus_get_parameters.                                  "n547170
*-----------------------------------------------------------"n547170
* Initialization of the user defaults for the checkboxes
* read the settings from table ESDUS
*-----------------------------------------------------------

* only in dialog mode
  check : sy-batch is initial.

  data : l_cnt_radiobutton   type i.

* get the parameters from the last run from table ESDUS as
* default values  in release 4.6 and higher

  if oref_settings is initial.
    create object oref_settings
      exporting
        i_action = 'RM07MLBD'.
  endif.

** get the parameters from the last run
  lgbst    = oref_settings->get( 'LGBST'  ).
  bwbst    = oref_settings->get( 'BWBST'  ).
  sbbst    = oref_settings->get( 'SBBST'  ).
  xchar    = oref_settings->get( 'XCHAR'  ).
  xsum     = oref_settings->get( 'XSUM'   ).
  pa_sumfl = oref_settings->get( 'PA_SUMFL'   ).
  nosto    = oref_settings->get( 'NOSTO'  ).
  pa_aistr  = oref_settings->get( 'PA_AISTR' ).             "n1481757

**  get the parameters for the list categories              "n599218
  pa_wdzer = oref_settings->get( 'PA_WDZER' ).              "n599218
  pa_wdzew = oref_settings->get( 'PA_WDZEW' ).              "n599218
  pa_wdwiz = oref_settings->get( 'PA_WDWIZ' ).              "n599218
  pa_wdwuw = oref_settings->get( 'PA_WDWUW' ).              "n599218
  pa_wdwew = oref_settings->get( 'PA_WDWEW' ).              "n599218
  pa_ndsto = oref_settings->get( 'PA_NDSTO' ).              "n599218
  pa_ndzer = oref_settings->get( 'PA_NDZER' ).              "n599218
  xnomchb  = oref_settings->get( 'XNOMCHB' ).               "838360

**  check radiobutton rules
  if  not lgbst is initial.
    add  1                 to  l_cnt_radiobutton.
  endif.

  if  not bwbst is initial.
    add  1                 to  l_cnt_radiobutton.
  endif.

  if  not sbbst is initial.
    add  1                 to  l_cnt_radiobutton.
  endif.

  if  l_cnt_radiobutton ne 1.
**    offend against radiobutton rules ?
**    yes -> set the first and delete the rest
    move : 'X'             to  lgbst.
    clear :                bwbst, sbbst.
  endif.

* at the first time ( or in a lower release ) all seven     "n599218
* list categories will be initial --> activate them all     "n599218
  perform                    f0850_empty_parameters.        "n599218
                                                            "n599218
  if  g_cnt_empty_parameter = 7.                            "n599218
    move : 'X'               to  pa_wdzer,                  "n599218
           'X'               to  pa_wdzew,                  "n599218
           'X'               to  pa_wdwiz,                  "n599218
           'X'               to  pa_wdwuw,                  "n599218
           'X'               to  pa_wdwew,                  "n599218
           'X'               to  pa_ndsto,                  "n599218
           'X'               to  pa_ndzer.                  "n599218
  endif.                                                    "n599218

endform.                     "esdus_get_parameters          "n547170

*-----------------------------------------------------------"n547170
*    esdus_save_parameters                                  "n547170
*-----------------------------------------------------------"n547170

form esdus_save_parameters.                                 "n547170
                                                            "n547170
* only in dialog mode
  check : sy-batch is initial.

* Save the settings in release 4.6 and higher
  call method oref_settings->set(
      i_element = 'LGBST'
      i_active  = lgbst ).
  call method oref_settings->set(
      i_element = 'BWBST'
      i_active  = bwbst ).
  call method oref_settings->set(
      i_element = 'SBBST'
      i_active  = sbbst ).
  call method oref_settings->set(
      i_element = 'XCHAR'
      i_active  = xchar ).
  call method oref_settings->set(
      i_element = 'XNOMCHB'    "838360
      i_active  = xnomchb ). "838360


*    CALL METHOD oref_settings->set( i_element = 'XONUL'
*                                    i_active  =  xonul   ).
*
*    CALL METHOD oref_settings->set( i_element = 'XVBST'
*                                    i_active  =  XVBST   ).
*    CALL METHOD oref_settings->set( i_element = 'XNVBST'
*                                    i_active  =  xnvbst  ).

*   save the list categories                                "n599218
  call method oref_settings->set(
      i_element = 'PA_WDZER'  "n599218
      i_active  = pa_wdzer ). "n599218
  call method oref_settings->set(
      i_element = 'PA_WDZEW'  "n599218
      i_active  = pa_wdzew ). "n599218
  call method oref_settings->set(
      i_element = 'PA_WDWIZ'  "n599218
      i_active  = pa_wdwiz ). "n599218
  call method oref_settings->set(
      i_element = 'PA_WDWUW'  "n599218
      i_active  = pa_wdwuw ). "n599218
  call method oref_settings->set(
      i_element = 'PA_WDWEW'  "n599218
      i_active  = pa_wdwew ). "n599218

  call method oref_settings->set(
      i_element = 'PA_NDSTO'  "n599218
      i_active  = pa_ndsto ). "n599218
  call method oref_settings->set(
      i_element = 'PA_NDZER'  "n599218
      i_active  = pa_ndzer ). "n599218

  call method oref_settings->set(
      i_element = 'XSUM'
      i_active  = xsum ).
  call method oref_settings->set(
      i_element = 'PA_SUMFL'
      i_active  = pa_sumfl ).

  call method oref_settings->set(
      i_element = 'NOSTO'
      i_active  = nosto ).

  call method oref_settings->flush.

*   carry out the database updates only; the normal commit  "n599218
*   command does not allow to record this transaction for   "n599218
*   a batch input session using transaction SHDB            "n599218
  call function 'DB_COMMIT'. "n599218

endform.                     "esdus_save_parameters         "n547170

*-----------------------------------------------------------"n547170
                                                            "n599218 A
*&---------------------------------------------------------------------*
*&      Form  check_is_oil_system
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form check_is_oil_system.                                   "n599218 A
                                                            "n599218 A
  move  'OI001'              to  g_f_dcobjdef_name.         "n599218 A
  clear : g_flag_is_oil_active, g_cnt_is_oil.               "n599218 A
                                                            "n599218 A
* does database OI001 exist in this system ?                "n599218 A
  call function 'DDIF_NAMETAB_GET'                         "n599218 A
    exporting                                               "n599218 A
      tabname   = g_f_dcobjdef_name                 "n599218 A
    tables                                                  "n599218 A
      x031l_tab = g_t_x031l                         "n599218 A
    exceptions                                              "n599218 A
      others    = 1.                                "n599218 A
                                                            "n599218 A
  check sy-subrc is initial.      "OI001 is available ?     "n599218 A
                                                            "n599218 A
* check definition of MM document item MSEG                 "n599218 A
  move  'MSEG'               to  g_f_dcobjdef_name.         "n599218 A
                                                            "n599218 A
  call function 'DDIF_NAMETAB_GET'                          "n599218 A
    exporting                                               "n599218 A
      tabname   = g_f_dcobjdef_name                 "n599218 A
    tables                                                  "n599218 A
      x031l_tab = g_t_x031l                         "n599218 A
    exceptions                                              "n599218 A
      others    = 1.                                "n599218 A
                                                            "n599218 A
  check sy-subrc is initial.      "structure MSEG found     "n599218 A
                                                            "n599218 A
* check whether the IS-OIL specific fields are available    "n599218 A
  loop at g_t_x031l          into  g_s_x031l.               "n599218 A
    case  g_s_x031l-fieldname.                              "n599218 A
      when  'OIGLCALC'.                                     "n599218 A
        add   1              to  g_cnt_is_oil.              "n599218 A
                                                            "n599218 A
      when  'OIGLSKU'.                                      "n599218 A
        add   2              to  g_cnt_is_oil.              "n599218 A
    endcase.                                                "n599218 A
  endloop.                                                  "n599218 A
                                                            "n599218 A
* in the case structure MSEG comprises both fields          "n599218 A
* -> activate the IS-OIL function                           "n599218 A
  if    g_cnt_is_oil = 3.                                   "n599218 A
    move  'X'                to  g_flag_is_oil_active.      "n599218 A
  endif.                                                    "n599218 A
                                                            "n599218 A
endform.                     "check_is_oil_system.          "n599218 A
                                                            "n599218 A
*----------------------------------------------------------------------*
*  calculate_offsets.
*----------------------------------------------------------------------*

* calculate the offsets for the list header

form calculate_offsets.

*  working area
  data : l_text(132)         type c.

* get the maximal length of the text elements to be used
  perform  get_max_text_length using  text-020.
  perform  get_max_text_length using  text-021.
  perform  get_max_text_length using  text-022.
  perform  get_max_text_length using  text-023.
  perform  get_max_text_length using  text-025.

  g_offset_header            =  g_f_length_max + 3.

  clear                      g_f_length_max.

  if  bwbst is initial.
*     stocks and quantities only
    move   text-007          to  g_date_line_from-text.
    write : datum-low        to  g_date_line_from-datum dd/mm/yyyy.
    condense                 g_date_line_from.
    perform  get_max_text_length using  g_date_line_from.

    move  text-005           to  g_text_line-text.
    perform  get_max_text_length using  g_text_line.

    move  text-006           to  g_text_line.
    perform  get_max_text_length using  g_text_line.

    move   text-007          to  g_date_line_to-text.
    write : datum-high       to  g_date_line_to-datum dd/mm/yyyy.
    condense                 g_date_line_to.
  else.
*     stocks, quantities, and values
    move   text-008          to  g_date_line_from-text.
    write : datum-low        to  g_date_line_from-datum dd/mm/yyyy.
    condense                 g_date_line_from.

*     the start and end dates were shown incorrectly in the "n856424
*     headlines in the mode valuated stock                  "n856424
    perform  get_max_text_length using  g_date_line_from.   "n856424

    move  text-030           to  g_text_line-text.
    perform  get_max_text_length using  g_text_line.

    move  text-031           to  g_text_line-text.
    perform  get_max_text_length using  g_text_line.

    move   text-008          to  g_date_line_to-text.
    write : datum-high       to  g_date_line_to-datum dd/mm/yyyy.
    condense                 g_date_line_to.
  endif.

* calculate the offsets for the following columns
  g_offset_qty               =  g_f_length_max +  2.
  g_offset_unit              =  g_offset_qty   + 25.
  g_offset_value             =  g_offset_unit  +  8.
  g_offset_curr              =  g_offset_value + 25.
  log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_19\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
* Offset for quantities in PUM
  gv_/cwm/offset_qty          =  g_offset_unit       +  8.
  gv_/cwm/offset_unit         =  gv_/cwm/offset_qty  + 25.

*  ENHANCEMENT-POINT ehp605_rm07mlbd_19 SPOTS es_rm07mlbd .

endform.                     " calculate_offsets.

*----------------------------------------------------------------------*
*    get_max_text_length
*----------------------------------------------------------------------*

form get_max_text_length         using l_text type any.

  g_f_length = strlen( l_text ).

  if  g_f_length > g_f_length_max.
    move  g_f_length         to  g_f_length_max.
  endif.

endform.                     " get_max_text_length

*----------------------------------------------------------------------*

* contains FORM routines without preprocessor commands and  "n547170
* no text elements                                          "n547170
include                      rm07mlbd_form_01.              "n547170

include                      rm07mlbd_form_02.              "n547170

*----------------------------------------------------------------------*
at selection-screen on value-request for pa_aistr.          "n1481757
                                                            "n1481757
* look and show the active archive info structures F4 help      "n1481757
                                                            "n1481757
  move   'X'                 to  g_f_f4_mode.               "n1481757
  export  g_f_f4_mode        to  memory id 'MB51_F4_MODE'.  "n1481757
                                                            "n1481757
* start this report in F4 mode without any parameters           "n1481757
  submit ('RM07DOCS')         and return.     "#EC CI_SUBMIT  "n1511550
                                                            "n1481757
* get the selected archive info structure                       "n1481757
  import  g_f_f4_archindex   from  memory                   "n1481757
                             id 'MB51_F4_ARCHINDEX'.        "n1481757
  move    g_f_f4_archindex   to  pa_aistr.                  "n1481757
                                                            "n1481757
  clear                      g_f_f4_mode.                   "n1481757
  export  g_f_f4_mode        to  memory id 'MB51_F4_MODE'.  "n1481757
                                                            "n1481757
* save archive info structure for the next run                  "n1481757
  if  archive   =  'X'.                                     "n1481757
    if  sy-batch is initial.  " only in dialog mode             "n1481757
                                                            "n1481757
      if not oref_settings is initial.                      "n1481757
*       this object is already known -> Save the settings       "n1481757
        call method                                         "n1481757
          oref_settings->set( i_element = 'PA_AISTR'        "n1481757
                              i_active  =  pa_aistr  ).     "n1481757
                                                            "n1481757
        call method oref_settings->flush. "n1481757
                                                            "n1481757
*       carry out the database updates only; the normal         "n1481757
*       commit command does not allow to record this            "n1481757
*       transaction for a batch input session using             "n1481757
*       transaction SHDB                                        "n1481757
        call function 'DB_COMMIT'. "n1481757
      endif.                                                "n1481757
    endif.                                                  "n1481757
  endif.                                                    "n1481757

*----- end of note 1481757 ----  F4-Help ----- get info-structure -----*


************************ HAUPTPROGRAMM *********************************

*---------------- F4-Hilfe für Reportvariante -------------------------*


*&--------------------------------------------       "v hana_20120802
*&      Form  NEW_DB_RUN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form new_db_run .

  refresh datum.  "delete existing entries because ...
  append datum.   ".. relevant data is appended here
  try.
      call badi gr_badi_rm07mlbd_dbsys_opt->calculate_stocks
        exporting
          it_material          = matnr[]
          it_company_code      = bukrs[]
          it_plant             = g_ra_werks[]                 "2053423
          it_storage_location  = lgort[]
          it_batch             = charg[]
          it_valuation_type    = bwtar[]
          it_movement_type     = bwart[]
          it_posting_date      = datum[]
          iv_special_stock_ind = sobkz
          iv_batch_stock       = lgbst
          iv_valuated_stock    = bwbst
          iv_special_stock     = sbbst
          iv_batch_mat_only    = xchar
          iv_batch_no_mchb     = xnomchb
          iv_no_reversals      = nosto
        importing
          et_stock_inventory   = gt_stock_inventory
          ev_no_dbsys_opt      = gv_no_dbsys_opt.
    catch cx_badi.
      if p_aut eq space.
* Code injection for ABAP UNIT TEST
* see local class of CL_IM_RM07MLBD_DBSYS_OPT
* The Unittest shall result in an error in case of error in BADI
        gv_newdb = abap_false.
      endif.
  endtry.
  if gv_no_dbsys_opt = abap_true.
    if p_aut eq space.
* Code injection for ABAP UNIT TEST
* see local class of CL_IM_RM07MLBD_DBSYS_OPT
* The Unittest shall result in an error in case of error in BADI
      gv_newdb = abap_false.
    endif.
  else.
    loop at gt_stock_inventory assigning <gs_stock_inventory>.
      move-corresponding <gs_stock_inventory> to bestand.
      move-corresponding <gs_stock_inventory> to g_s_makt.
      append bestand.
      append g_s_makt to g_t_makt.
      clear <gs_stock_inventory>-maktx. "to compare it to bestand in AUT
    endloop.
* if result is empty, call subroutines to get the detailed error messages "1784986
    if gv_newdb = abap_true and sy-subrc ne 0.              "1784986
      gv_newdb = abap_false.                                "1784986
      perform aktuelle_bestaende.                           "1784986
      perform f1000_select_mseg_mkpf.                       "1784986
      gv_newdb = abap_true.                                 "1784986
    endif.                                                  "1784986
    sort g_t_makt by matnr.
    delete adjacent duplicates from g_t_makt.
  endif.

endform.                    " new_db_run             "^ hana_20120802
*&---------------------------------------------------------------------*
*&      Form  Check_Ui_opti_Badi
*&---------------------------------------------------------------------*
*       check active implementation for UI enhancement note  1790231
*----------------------------------------------------------------------*
form check_ui_opti_badi.                                    "1790231

* check if BADI has been activated
  data: lo_ui_opti_badi type ref to mm_ui_optimizations.

  get badi lo_ui_opti_badi.
  call badi lo_ui_opti_badi->is_active
    exporting
      iv_reportname = sy-repid
    receiving
      rv_active     = gv_ui_opt_active.

endform.                    "Check_Ui_opti_Badi             "1790231
*&---------------------------------------------------------------------*
*& Form set_p_grid
*&---------------------------------------------------------------------*
*& Classic UI Harmonization for S/4HC - Replace ALV lists with ALV grids
*&---------------------------------------------------------------------*
form set_p_grid.

  if gv_ui_opt_active = abap_true and cl_cos_utilities=>is_cloud( ) = abap_true.

    p_grid = abap_true."as default for CE

    loop at screen.
      case screen-group1.
        when  'OPT'.
          screen-active = 1.
          screen-input  = 0.
          modify screen.
      endcase.
    endloop.

  endif.

endform.

form preenche_ksl .
  data    : l_tabix  like  sy-tabix,
            vdocln   like  faglflexa-docln,
            vksl     like  faglflexa-ksl,
            vg_dmbtr like faglflexa-ksl.

  loop at g_t_mseg_lean   into  g_s_mseg_lean.
    l_tabix = sy-tabix.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = g_s_mseg_lean-buzei
      importing
        output = vdocln.

    if vdocln is initial or vdocln = '000000'.
      if g_s_mseg_lean-shkzg = 'H'.
        vg_dmbtr = g_s_mseg_lean-dmbtr * -1.
      else.
        vg_dmbtr = g_s_mseg_lean-dmbtr.
      endif.
      select single ksl
        from faglflexa
        into vksl
        where belnr = g_s_mseg_lean-belnr
        and   rbukrs = g_s_mseg_lean-bukrs
        and   rldnr = '0L'
        and   hsl   = vg_dmbtr.
    else.
      select single ksl
        from faglflexa
        into vksl
        where belnr = g_s_mseg_lean-belnr
        and   docln = vdocln
        and   rbukrs = g_s_mseg_lean-bukrs
        and   rldnr = '0L'.
    endif.
    if sy-subrc eq 0.
      move vksl to g_s_mseg_lean-ksl.
      modify g_t_mseg_lean index l_tabix from g_s_mseg_lean transporting ksl.
    endif.

  endloop.
endform.
