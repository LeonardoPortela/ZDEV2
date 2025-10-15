*----------------------------------------------------------------------*
* INCLUDE BILAI00_2, used in RFBILA00                              *
* Select-Options und Parameters für Formulardruck aus altem
*           altem INCLUDE  BILA&I00.
*---------------------------------------------------------------------*

 SELECTION-SCREEN ULINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN:
*  COMMENT 01(20) TEXT-119 FOR FIELD SCR_FORM.
 PARAMETERS: SCR_FORM LIKE RFPDO2-BILAFORM.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN:
*  COMMENT 01(13) TEXT-121 FOR FIELD SCR_DEVI.
 PARAMETERS:  SCR_DEVI LIKE RFPDO2-BILADEVI.
*SELECTION-SCREEN END OF LINE.











