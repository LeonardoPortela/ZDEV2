"Name: \PR:RFFOBR_U\FO:DATEI_OEFFNEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_F110_PROP.
*
   IF reguh-XVORL = 'X'. "propostas ALRS
    clear gd_filename_new.
    DATA: T_SET TYPE STANDARD TABLE OF SETLEAF  WITH HEADER LINE.
    DATA: T_LAY TYPE STANDARD TABLE OF SETLINET WITH HEADER LINE.
    refresh: T_SET,T_LAY.
    SELECT *
    FROM  SETLEAF
    INTO TABLE T_SET
    WHERE SETCLASS      = '0000'
    AND   SETNAME        = 'MAGGI_PGT_F110'.

   if T_SET[] is not INITIAL.
       SELECT *
        FROM SETLINET
        INTO TABLE T_LAY
        FOR ALL ENTRIES IN T_SET
        WHERE SETCLASS   = T_SET-SETCLASS
        AND SUBCLASS     = T_SET-SUBCLASS
        AND SETNAME      = T_SET-SETNAME
        "AND LANGU        = 'P'
        AND LINEID       = T_SET-LINEID.
    endif.

    IF T_LAY[] is not INITIAL.
        READ TABLE T_SET INDEX 1.
        IF SY-SUBRC = 0.
           READ TABLE T_LAY WITH KEY  SETCLASS   = T_SET-SETCLASS
                                      SUBCLASS   = T_SET-SUBCLASS
                                      SETNAME    = T_SET-SETNAME
                                      LINEID     = T_SET-LINEID.
            IF SY-SUBRC = 0.
              CONCATENATE T_LAY-DESCRIPT reguh-LAUFI reguh-RZAWE '.txt'  into gd_filename_new.
            Endif.
        endif.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
