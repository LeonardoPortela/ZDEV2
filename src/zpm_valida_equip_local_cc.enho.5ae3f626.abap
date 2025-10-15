"Name: \PR:SAPLCOIH\FO:OBJECT_STATUS_CHECK\SE:BEGIN\EI
ENHANCEMENT 0 ZPM_VALIDA_EQUIP_LOCAL_CC.
"167603 Implementar check tipo de ordem PJ01 PSA

DATA: _kostl TYPE kostl.
DATA: _equnr TYPE equnr.
DATA: _tplnr TYPE tplnr.
DATA: wa_return TYPE bapi_itob.

IF header_new-tplnr IS NOT INITIAL.                             "1173061
  CLEAR: wa_return,_tplnr.
  _tplnr = header_new-tplnr.
  CALL FUNCTION 'BAPI_FUNCLOC_GETDETAIL'
    EXPORTING
      functlocation    = _tplnr
    IMPORTING
      data_general_exp = wa_return.

  IF sy-subrc = 0.
    CLEAR:_kostl.
    CONDENSE wa_return-costcenter NO-GAPS.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_return-costcenter
      IMPORTING
        output = _kostl.

    IF _kostl IS NOT INITIAL AND _kostl+6(4) <> '0196' AND header_new-auart = 'PJ01'.
      MESSAGE e024(sd) WITH 'CC LocInstal. deve ser Engenharia para ord. PJ01!'.
    ENDIF.

  ENDIF.


ENDIF.

IF header_new-equnr IS NOT INITIAL.                             "1173061

  CLEAR: wa_return,_equnr.
  _equnr = header_new-equnr.
  CONDENSE _equnr NO-GAPS.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = _equnr
    IMPORTING
      output = _equnr.

  CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
    EXPORTING
      equipment        = _equnr
    IMPORTING
      data_general_exp = wa_return.
  IF sy-subrc = 0.
    CLEAR:_kostl.
    CONDENSE wa_return-costcenter NO-GAPS.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_return-costcenter
      IMPORTING
        output = _kostl.

    IF _kostl IS NOT INITIAL AND _kostl+6(4) <> '0196' AND header_new-auart = 'PJ01'.
      MESSAGE e024(sd) WITH 'CC Equipam. deve ser Engenharia para ord. PJ01'.
    ENDIF.

  ENDIF.
ENDIF.

ENDENHANCEMENT.
