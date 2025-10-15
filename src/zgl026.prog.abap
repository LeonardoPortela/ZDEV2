*&---------------------------------------------------------------------*
*& Report  ZGL026
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZGL026.
CALL FUNCTION 'BMENU_START_BROWSER'
  EXPORTING
   mode                             = 'D'
    tree_id                          = 'ZDRE'    "(name of your menu)
EXCEPTIONS
   tree_does_not_exist              = 1
   no_authority                     = 2
   OTHERS                           = 3.
