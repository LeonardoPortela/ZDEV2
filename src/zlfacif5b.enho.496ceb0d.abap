"Name: \PR:SAPLFACI\FO:BELEGNUMMER_VERGEBEN\SE:END\EI
ENHANCEMENT 0 ZLFACIF5B.
*break etavares.

*data: t_lines like RGSBV OCCURS 0 WITH HEADER LINE.
if sy-tcode = 'MR22'.
data: BEGIN OF t_lines OCCURS 0.
  include STRUCTURE RGSBV.
data: end of t_lines.

data: Vl_id(3).

clear vl_id.
  import vl_id from memory id 'ZCO'.
free memory id 'ZCO'.
loop at xbseg.


*  if not vl_id is initial.
*    if xbseg-bschl = '83' or xbseg-bschl = '93'.
*         xbseg-ALTKT = xbseg-hkont = '0000114997'.
*    endif.
*  endif.



  if xbseg-hkont = '0000511005'.

    CALL FUNCTION 'G_SET_FETCH'
      EXPORTING
        SETNR                     = '0000MR22'
     TABLES
       SET_LINES_BASIC           = t_lines
     EXCEPTIONS
       NO_AUTHORITY              = 1
       SET_IS_BROKEN             = 2
       SET_NOT_FOUND             = 3
       OTHERS                    = 4
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

FIELD-SYMBOLS: <campo> type any.
ASSIGN ('(SAPRCKM_MR22)MR21HEAD-XBLNR') to <campo>.

    read table t_lines with key from = <campo>.
    if sy-subrc = 0.
        xbseg-hkont = t_lines-TITLE.
    else.
        read table t_lines with key from = ''.
        xbseg-hkont = t_lines-TITLE.
    endif.
  endif.

  MODIFY xbseg.
  endloop.
endif.

ENDENHANCEMENT.
