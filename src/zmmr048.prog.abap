*&---------------------------------------------------------------------*
*& Report  ZMMR048
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMMR048.

CALL FUNCTION 'BMENU_START_BROWSER'
  EXPORTING
    MODE                = 'D'
    TREE_ID             = 'ZWFMAT'    "(name of your menu)
  EXCEPTIONS
    TREE_DOES_NOT_EXIST = 1
    NO_AUTHORITY        = 2
    OTHERS              = 3.
