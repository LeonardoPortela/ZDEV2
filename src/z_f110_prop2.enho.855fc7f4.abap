"Name: \PR:RFFOZA_A\FO:DATEI_OEFFNEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_F110_PROP2.
*
  IF *regut-XVORL = 'X'.
    DATA: T_PATH            TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.
    refresh T_PATH.
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        CLASS         = '0000'
        SETNR         = 'MAGGI_PGT_F110'
      TABLES
        SET_VALUES    = T_PATH
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    SORT T_PATH BY FROM.
    IF t_path[] is not INITIAL.
       READ TABLE t_path INDEX 1.
       gd_filename_new = t_path-from.
    ENDIF.


  ENDIF.
ENDENHANCEMENT.
