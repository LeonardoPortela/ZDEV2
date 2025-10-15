FUNCTION z_busca_juros_multa_boleto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BELNR) TYPE  BELNR_D
*"     VALUE(I_BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     VALUE(E_ORIGEM) TYPE  CHAR30
*"     VALUE(E_JUROS) TYPE  ZDE003
*"     VALUE(E_MULTA) TYPE  ZDE003
*"     VALUE(E_NRO_SOL_OV) TYPE  ZSDED013
*"     VALUE(E_SEQ_LCTO) TYPE  ZFIWED006
*"----------------------------------------------------------------------

  DATA: lva_num1(10) TYPE c,
        lva_deci(02) TYPE c.

  DATA: lva_text          TYPE string,
        lva_amount        TYPE string,
        lva_num           TYPE i,
        lva_doc_simulacao TYPE zsdt0040-doc_simulacao.

  TYPES:
    BEGIN OF ty_bkpf_62,
      belnr TYPE bkpf-belnr,
      awkey TYPE vbrp-vbeln,
    END OF ty_bkpf_62,

    BEGIN OF ty_znfw,
      bukrs   TYPE bkpf-bukrs,
      belnr   TYPE bkpf-belnr,
      obj_key TYPE zfiwrt0008-obj_key,
    END OF ty_znfw.

  DATA: lit_bkpf       TYPE TABLE OF bkpf,
        lit_bkpf_62    TYPE TABLE OF ty_bkpf_62,
        lit_vbrp       TYPE TABLE OF vbrp,
        lit_zsdt0053   TYPE TABLE OF zsdt0053,
        lit_zsdt0054   TYPE TABLE OF zsdt0054,
        lit_zsdt0051   TYPE TABLE OF zsdt0051,
        lit_znfw       TYPE TABLE OF ty_znfw,
        lit_zfiwrt0008 TYPE TABLE OF zfiwrt0008,
        lit_zfiwrt0011 TYPE TABLE OF zfiwrt0011.

  DATA: lwa_bkpf       TYPE bkpf,
        lwa_bkpf_62    TYPE ty_bkpf_62,
        lwa_znfw       TYPE ty_znfw,
        lwa_zsdt0051   TYPE zsdt0051,
        lwa_zfiwrt0011 TYPE zfiwrt0011,
        lwa_zsdt0040   TYPE zsdt0040.


  SELECT  *
    FROM  bkpf
    INTO TABLE lit_bkpf
    WHERE bukrs EQ i_bukrs
      AND belnr EQ i_belnr.
"ajuste verificar se retornou dados da bkpf #118209 - BG
  IF lit_bkpf[] IS NOT INITIAL.

    LOOP AT lit_bkpf INTO lwa_bkpf.
      MOVE lwa_bkpf-awkey TO lwa_bkpf_62-awkey.
      lwa_bkpf_62-belnr = lwa_bkpf-belnr.

      APPEND lwa_bkpf_62 TO lit_bkpf_62.
      CLEAR: lwa_bkpf_62.
    ENDLOOP.

    SELECT  *
      FROM  vbrp
      INTO TABLE lit_vbrp
             FOR ALL ENTRIES IN lit_bkpf_62
      WHERE vbeln = lit_bkpf_62-awkey.

    IF lit_vbrp IS NOT INITIAL.
      SELECT  *
       FROM  zsdt0053
       INTO TABLE lit_zsdt0053
              FOR ALL ENTRIES IN lit_vbrp
       WHERE vbeln = lit_vbrp-aubel.

      SELECT  *
       FROM  zsdt0051
       INTO TABLE lit_zsdt0051
              FOR ALL ENTRIES IN lit_zsdt0053
       WHERE nro_sol_ov = lit_zsdt0053-nro_sol_ov.
    ELSE.
      READ TABLE lit_bkpf INTO DATA(wl_lit_bkpf) INDEX 1.
      IF wl_lit_bkpf-bstat EQ 'S'.
        SELECT  *
         FROM  zsdt0054
         INTO TABLE lit_zsdt0054
                FOR ALL ENTRIES IN lit_bkpf
         WHERE adiant = lit_bkpf-belnr.

        SELECT  *
         FROM  zsdt0051
         INTO TABLE lit_zsdt0051
                FOR ALL ENTRIES IN lit_zsdt0054
         WHERE nro_sol_ov = lit_zsdt0054-nro_sol_ov.
      ENDIF.
    ENDIF.
  ENDIF.
"fim ajuste #118209 - BG
* ZSDT0044 - Inicio
  lva_num = strlen( wl_lit_bkpf-xblnr ).
  lva_text = wl_lit_bkpf-xblnr.

  DO lva_num TIMES.
    IF lva_text(1) CA '0123456789'.
      CONCATENATE lva_amount lva_text(1) INTO lva_amount.
      CONDENSE lva_amount NO-GAPS.
    ENDIF.
    SHIFT lva_text LEFT CIRCULAR.
  ENDDO.

  MOVE lva_amount TO lva_doc_simulacao.
  SELECT SINGLE * INTO lwa_zsdt0040
    FROM zsdt0040
  WHERE doc_simulacao = lva_doc_simulacao.

****************************JUROS*****************************************
  IF lwa_zsdt0040-juros_ano > 0.
    e_origem = 'ZSDT0044'.
    e_nro_sol_ov = lwa_zsdt0040-doc_simulacao.
    MOVE lwa_zsdt0040-juros_ano TO e_juros.
  ELSEIF lit_zsdt0051 IS NOT INITIAL.
    READ TABLE lit_zsdt0051 INTO lwa_zsdt0051 INDEX 1.
    IF lwa_zsdt0051-tx_juros > 0.
      e_origem = 'ZSDT0062'.
      e_nro_sol_ov = lwa_zsdt0051-nro_sol_ov.
      MOVE lwa_zsdt0051-tx_juros TO e_juros.
    ENDIF.
  ELSE.
    LOOP AT lit_bkpf INTO lwa_bkpf.
      MOVE lwa_bkpf-awkey TO lwa_znfw-obj_key.

      IF lwa_znfw-obj_key IS NOT INITIAL.
        lwa_znfw-bukrs = lwa_bkpf-bukrs.
        lwa_znfw-belnr = lwa_bkpf-belnr.

        APPEND lwa_znfw TO lit_znfw.
        CLEAR: lwa_znfw.
      ENDIF.
    ENDLOOP.

    IF lit_znfw IS NOT INITIAL.
      SELECT  *
      FROM   zfiwrt0008
      INTO TABLE lit_zfiwrt0008
             FOR ALL ENTRIES IN lit_znfw
      WHERE obj_key EQ lit_znfw-obj_key
        AND bukrs = lit_znfw-bukrs .

      IF lit_zfiwrt0008 IS NOT INITIAL.
        SELECT  *
          FROM zfiwrt0011
       INTO TABLE lit_zfiwrt0011
          FOR ALL ENTRIES IN lit_zfiwrt0008
            WHERE seq_lcto  EQ lit_zfiwrt0008-seq_lcto
              AND zlsch EQ 'D'
              AND estorno NE 'X'.
      ENDIF.
    ENDIF.

    IF lit_zfiwrt0011 IS NOT INITIAL.
      READ TABLE lit_zfiwrt0011 INTO lwa_zfiwrt0011 INDEX 1.
      IF sy-subrc EQ 0.
        e_origem = 'ZNFW'.
        e_seq_lcto = lwa_zfiwrt0011-seq_lcto.
        IF lwa_zfiwrt0011-taxa_juros > 0.
          MOVE lwa_zfiwrt0011-taxa_juros TO e_juros.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*****************************MULTA***************************************
  IF lit_zsdt0051 IS NOT INITIAL.
    READ TABLE lit_zsdt0051 INTO lwa_zsdt0051 INDEX 1.
    IF lwa_zsdt0051-tx_multa > 0.
      IF e_origem IS INITIAL.
        e_origem = 'ZSDT0062'.
        e_nro_sol_ov = lwa_zsdt0051-nro_sol_ov.
      ENDIF.
      MOVE lwa_zsdt0051-tx_multa TO e_multa.
    ENDIF.
  ELSE.
    LOOP AT lit_bkpf INTO lwa_bkpf.
      MOVE lwa_bkpf-awkey TO lwa_znfw-obj_key.

      IF lwa_znfw-obj_key IS NOT INITIAL.
        lwa_znfw-bukrs = lwa_bkpf-bukrs.
        lwa_znfw-belnr = lwa_bkpf-belnr.

        APPEND lwa_znfw TO lit_znfw.
        CLEAR: lwa_znfw.
      ENDIF.
    ENDLOOP.

    IF lit_znfw IS NOT INITIAL.
      SELECT  *
      FROM   zfiwrt0008
      INTO TABLE lit_zfiwrt0008
             FOR ALL ENTRIES IN lit_znfw
      WHERE obj_key EQ lit_znfw-obj_key
        AND bukrs = lit_znfw-bukrs .

      IF lit_zfiwrt0008 IS NOT INITIAL.
        SELECT  *
          FROM zfiwrt0011
       INTO TABLE lit_zfiwrt0011
          FOR ALL ENTRIES IN lit_zfiwrt0008
            WHERE seq_lcto  EQ lit_zfiwrt0008-seq_lcto
              AND zlsch EQ 'D'
              AND estorno NE 'X'.
      ENDIF.
    ENDIF.

    IF lit_zfiwrt0011 IS NOT INITIAL.
      READ TABLE lit_zfiwrt0011 INTO lwa_zfiwrt0011 INDEX 1.
      IF sy-subrc EQ 0.
        IF e_origem IS INITIAL.
          e_origem = 'ZNFW'.
          e_seq_lcto = lwa_zfiwrt0011-seq_lcto.
        ENDIF.
        IF lwa_zfiwrt0011-taxa_multa > 0.
          MOVE lwa_zfiwrt0011-taxa_multa TO e_multa.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
