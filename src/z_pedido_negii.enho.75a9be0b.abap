"Name: \PR:SAPLMBGB\FO:WERE_BETRAG\SE:BEGIN\EI
ENHANCEMENT 0 Z_PEDIDO_NEGII.
*
  data T_PEDIDO                TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'MAGGI_PEDIDO_NEG'
    TABLES
      SET_VALUES    = T_PEDIDO
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT T_PEDIDO BY FROM.

  READ TABLE T_PEDIDO with key from = XEKWS-EBELN.
  IF sy-subrc = 0.

      IF ( mseg-shkzg = s AND vm07m-retpo IS INITIAL ) OR
         ( mseg-shkzg = h AND NOT vm07m-retpo IS INITIAL ).
        PERFORM we_zugang USING  w-netwr
                                dm07m-remng
                                dm07m-bprem
                                dm07m-arewr.
      ELSE.
         PERFORM we_abgang USING w-wemng
                            w-wewrt
                            w-remng
                            w-rewrt.
      ENDIF.
      "
       IF betrag > maximum OR betrag < minimum.
        MESSAGE e302 WITH 'WERE_BETRAG' 'BETRAG'.
      ENDIF.
      vwere = betrag.
      refe1 = vwere + fracht + lbwert.

*      IF refe1 < 0.
*        MESSAGE e301.
*      ENDIF.

      fwere = vwere = 0 - vwere.
      exbualt = - vwere + fracht + exalt.                           "1820941
      IF exbualt > maximum OR exbualt < minimum.
        MESSAGE e302 WITH 'WERE_BETRAG' 'EXBUALT'.
      ENDIF.
      betrag = - vwere + fracht + lbwert.                           "1820941
      IF betrag > maximum OR betrag < minimum.
        MESSAGE e302 WITH 'WERE_BETRAG' 'BETRAG2'.
      ENDIF.

      EXIT.

  endif.
ENDENHANCEMENT.
