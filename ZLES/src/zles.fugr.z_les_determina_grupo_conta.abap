FUNCTION z_les_determina_grupo_conta.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_LIFNR) TYPE  LIFNR OPTIONAL
*"     VALUE(P_PLACA) TYPE  ZPC_VEICULO OPTIONAL
*"     REFERENCE(P_TKNUM) TYPE  TKNUM
*"  EXPORTING
*"     VALUE(O_ADD02) TYPE  VTTK_ADD02
*"     VALUE(O_LIFNR) TYPE  LIFNR
*"  TABLES
*"      C_XVBPA STRUCTURE  VBPAVB OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: vl_grpcta    TYPE ktokk,
        vl_indtyp    TYPE indtyp,
        wa_zlest0002 TYPE zlest0002,
        wk_vbpa      TYPE vbpavb,
        wa_lfa1      TYPE lfa1.

  IF p_lifnr IS INITIAL AND NOT p_placa IS INITIAL.

    SELECT SINGLE * INTO wa_zlest0002
      FROM zlest0002
     WHERE pc_veiculo EQ p_placa.

    IF ( sy-subrc IS INITIAL ) AND ( NOT wa_zlest0002-proprietario IS INITIAL ).
      o_lifnr = wa_zlest0002-proprietario.

      READ TABLE c_xvbpa INTO wk_vbpa WITH KEY parvw = 'PV'.
      IF NOT sy-subrc IS INITIAL.

        SELECT SINGLE * INTO wa_lfa1
          FROM lfa1
         WHERE lifnr EQ o_lifnr.

        wk_vbpa-mandt = sy-mandt.
        wk_vbpa-parvw = 'PV'.
        wk_vbpa-lifnr = wa_lfa1-lifnr.
        wk_vbpa-adrnr = wa_lfa1-adrnr.
        wk_vbpa-land1 = wa_lfa1-land1.
        wk_vbpa-vbeln = p_tknum.
        wk_vbpa-adrda = 'D'.
        wk_vbpa-name1 = wa_lfa1-name1.
        wk_vbpa-nrart = 'LI'.
        wk_vbpa-updkz = 'I'.
        wk_vbpa-fehgr = '08'.
        wk_vbpa-spras = sy-langu.
        APPEND wk_vbpa TO c_xvbpa.
      ENDIF.

    ELSE.
      MESSAGE s073(zles) RAISING error.
    ENDIF.
  ELSE.
    o_lifnr = p_lifnr.
  ENDIF.

  SELECT SINGLE ktokk
  INTO vl_grpcta
  FROM lfa1
  WHERE lifnr = o_lifnr.

  SELECT SINGLE indtyp
  INTO vl_indtyp
  FROM lfa1
  WHERE lifnr = o_lifnr.

  IF sy-subrc IS INITIAL.
    CASE vl_grpcta.
      WHEN 'ZFFF' OR 'ZFNF' OR 'ZPRF'.
        o_add02 = '0000000003'.
      WHEN 'ZFFJ' OR 'ZFNJ' OR 'ZPRJ'.
        IF vl_indtyp = 'Z3'. "PBI - 62688
          o_add02 = '0000000004'.
        ELSE.
          o_add02 = '0000000002'.
        ENDIF.
      WHEN 'ZFIC'.
        o_add02 = '0000000001'.
      WHEN OTHERS.
        MESSAGE s011(zles) RAISING error.
    ENDCASE.
  ELSE.
    MESSAGE s011(zles) RAISING error.
  ENDIF.

ENDFUNCTION.
