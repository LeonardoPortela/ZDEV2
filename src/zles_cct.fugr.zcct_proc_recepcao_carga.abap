FUNCTION zcct_proc_recepcao_carga.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_GRAVAR_REGISTRO) TYPE  CHAR01
*"     REFERENCE(I_RECINTO_TERCEIRO) TYPE  CHAR01 OPTIONAL
*"  CHANGING
*"     REFERENCE(C_ZLEST0146) TYPE  ZLEST0146
*"     REFERENCE(C_ZLEST0147) TYPE  ZLEST0147
*"     REFERENCE(C_ZLEST0168) TYPE  ZLEST0168_T
*"     REFERENCE(C_RETORNO) TYPE  ZDE_RETORNO_PROC
*"----------------------------------------------------------------------

  DATA: tg_lfa1       TYPE TABLE OF lfa1      WITH HEADER LINE.

  DATA: v_msg    TYPE string,
        v_candat TYPE j_1bnfdoc-candat.

  DATA: tg_0146      TYPE TABLE OF zlest0146 WITH HEADER LINE,
        wl_doc_tmp   TYPE j_1bnfdoc,
        wl_zlest0146 TYPE zlest0146,
        wl_zlest0147 TYPE zlest0147,
        v_chave_nfe  TYPE zde_chave_nfe,
        v_chave_nff  TYPE zde_chave_nff.

  DATA: wl_set_cons_nfe_val_nf   TYPE setleaf,
        wl_set_cons_nfe_val_urf  TYPE setleaf,
        wl_set_cons_nfe_val_ra   TYPE setleaf,
        wl_set_cons_nfe_val_peso TYPE setleaf.

  CLEAR: c_zlest0168, c_retorno.

*--------------------------------------------------------------------------*
*   Conversion EXIT
*--------------------------------------------------------------------------*
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_zlest0147-serie
    IMPORTING
      output = c_zlest0147-serie.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_zlest0147-nfnum9
    IMPORTING
      output = c_zlest0147-nfnum9.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_zlest0147-nfnum
    IMPORTING
      output = c_zlest0147-nfnum.


  IF c_zlest0147-model IS INITIAL.
    MESSAGE s085 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF ( c_zlest0147-model NE '55' ) AND
     ( c_zlest0147-model NE '01' ) AND
     ( c_zlest0147-model NE '04' ).
    MESSAGE s086 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  CASE c_zlest0147-model.
    WHEN '55'.

      IF c_zlest0147-nfnum9 IS INITIAL.
        MESSAGE s116 INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      CLEAR: wl_set_cons_nfe_val_nf, wl_set_cons_nfe_val_urf, wl_set_cons_nfe_val_ra.

      SELECT SINGLE *
        FROM setleaf INTO wl_set_cons_nfe_val_nf
       WHERE setname EQ 'ZLES0164_CONS_NFE'
         AND valfrom EQ 'VAL_NFE'.

      SELECT SINGLE *
        FROM setleaf INTO wl_set_cons_nfe_val_urf
       WHERE setname EQ 'ZLES0164_CONS_NFE'
         AND valfrom EQ 'VAL_URF'.

      SELECT SINGLE *
        FROM setleaf INTO wl_set_cons_nfe_val_ra
       WHERE setname EQ 'ZLES0164_CONS_NFE'
         AND valfrom EQ 'VAL_RA'.

      SELECT SINGLE *
        FROM setleaf INTO wl_set_cons_nfe_val_peso
       WHERE setname EQ 'ZLES0164_CONS_NFE'
         AND valfrom EQ 'VAL_PESO'.

      SELECT SINGLE *
        FROM zlest0186 INTO @DATA(_wl_zlest0168)
       WHERE chave EQ @c_zlest0147-chave_nfe.

      IF ( sy-subrc NE 0 ) AND ( wl_set_cons_nfe_val_nf-valfrom IS NOT INITIAL ).
        MESSAGE s136 WITH c_zlest0147-chave_nfe INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      IF ( c_zlest0146-local_codigo_urf <> _wl_zlest0168-codigo_urf ) AND ( wl_set_cons_nfe_val_urf-valfrom IS NOT INITIAL ).
        MESSAGE s137 WITH c_zlest0147-chave_nfe  _wl_zlest0168-codigo_urf c_zlest0146-local_codigo_urf INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      IF ( c_zlest0146-local_codigo_ra <> _wl_zlest0168-codigo_ra ) AND ( wl_set_cons_nfe_val_ra-valfrom IS NOT INITIAL ).
        MESSAGE s138 WITH c_zlest0147-chave_nfe  _wl_zlest0168-codigo_ra c_zlest0146-local_codigo_ra INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      IF ( c_zlest0146-peso_aferido_recepcao <> _wl_zlest0168-peso_aferido ) AND ( wl_set_cons_nfe_val_peso-valfrom IS NOT INITIAL ).
        MESSAGE s159 WITH c_zlest0147-chave_nfe  _wl_zlest0168-peso_aferido c_zlest0146-peso_aferido_recepcao INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

    WHEN OTHERS.
      IF c_zlest0147-nfnum IS INITIAL.
        MESSAGE s116 INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.
  ENDCASE.

*--------------------------------------------------------------------------*
*   Validações
*--------------------------------------------------------------------------*
  IF c_zlest0146-cnpj_responsavel IS INITIAL.
    MESSAGE s087 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF c_zlest0146-local_codigo_urf IS INITIAL.
    MESSAGE s088 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF c_zlest0146-local_codigo_ra IS INITIAL.
    MESSAGE s089 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF ( c_zlest0146-local_codigo_urf IS INITIAL ) OR ( c_zlest0146-local_codigo_ra IS INITIAL ).
    IF c_zlest0146-local_latitude IS INITIAL.
      MESSAGE s090 INTO v_msg.
      c_retorno-type     = 'E'.
      c_retorno-msgno    = sy-msgno.
      c_retorno-texto    = v_msg.
      RETURN.
    ENDIF.

    IF c_zlest0146-local_longitude IS INITIAL.
      MESSAGE s091 INTO v_msg.
      c_retorno-type     = 'E'.
      c_retorno-msgno    = sy-msgno.
      c_retorno-texto    = v_msg.
      RETURN.
    ENDIF.
  ENDIF.

  IF ( i_recinto_terceiro EQ abap_true ).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_zlest0146-local_codigo_ra
      IMPORTING
        output = c_zlest0146-local_codigo_ra.

    SELECT SINGLE *
      FROM zsdt0169 INTO @DATA(_wl_0169)
     WHERE codigo_ra EQ @c_zlest0146-local_codigo_ra.

    IF sy-subrc NE 0.
      MESSAGE s113 INTO v_msg.
      c_retorno-type     = 'E'.
      c_retorno-msgno    = sy-msgno.
      c_retorno-texto    = v_msg.
      RETURN.
    ENDIF.
  ENDIF.

  IF c_zlest0147-dt_emissao IS INITIAL.
    MESSAGE s092 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF ( c_zlest0146-transportador_cnpj IS INITIAL ) AND
     ( c_zlest0146-transportador_cpf  IS INITIAL ).
    MESSAGE s093 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF ( c_zlest0147-sigla_uf_emissor IS INITIAL ) AND ( c_zlest0147-model NE '55' ).
    MESSAGE s094 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF c_zlest0146-peso_aferido_recepcao <= 0.
    MESSAGE s095 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  IF c_zlest0146-dt_recepcao IS INITIAL.
    MESSAGE s096 INTO v_msg.
    c_retorno-type     = 'E'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  CASE c_zlest0147-model.
    WHEN '55'.

      IF ( c_zlest0147-emissor_cnpj IS INITIAL ) AND ( c_zlest0147-emissor_cpf IS INITIAL ).
        MESSAGE s097 INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      IF c_zlest0147-chave_nfe IS INITIAL.
        MESSAGE s098 INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      IF strlen( c_zlest0147-chave_nfe ) NE 44.
        MESSAGE s099 INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      c_zlest0147-regio            = c_zlest0147-chave_nfe(2).
      c_zlest0147-docnum9          = c_zlest0147-chave_nfe+34(9).
      c_zlest0147-cdv              = c_zlest0147-chave_nfe+43(1).

      DATA(_emissor_proprio) = abap_false.

      "Check se Emissão é propria ou Terceiro
      CLEAR: tg_lfa1[].

      IF c_zlest0147-emissor_cnpj IS NOT INITIAL.

        SELECT *
          FROM lfa1 INTO TABLE tg_lfa1
         WHERE stcd1 EQ c_zlest0147-emissor_cnpj.

      ELSEIF c_zlest0147-emissor_cpf IS NOT INITIAL.

        SELECT *
          FROM lfa1 INTO TABLE tg_lfa1
         WHERE stcd2 EQ c_zlest0147-emissor_cpf.

      ENDIF.

      "Elimina Fornecedores Bloqueados
      PERFORM f_elimina_lfa1_bloq TABLES tg_lfa1.

      IF tg_lfa1[] IS INITIAL.

        IF c_zlest0147-emissor_cnpj IS NOT INITIAL.
          MESSAGE s100 WITH c_zlest0147-emissor_cnpj INTO v_msg.
        ELSE.
          MESSAGE s100 WITH c_zlest0147-emissor_cpf INTO v_msg.
        ENDIF.

        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      IF c_zlest0147-emissor_ie IS INITIAL.
        LOOP AT tg_lfa1 WHERE stcd3 IS NOT INITIAL.
          c_zlest0147-emissor_ie = tg_lfa1-stcd3.
          EXIT.
        ENDLOOP.
      ENDIF.

      READ TABLE tg_lfa1 WITH KEY ktokk = 'ZFIC'.
      IF sy-subrc = 0.
        _emissor_proprio = abap_true.
      ENDIF.

      IF _emissor_proprio = abap_true.
        PERFORM f_get_documentos_recepcao USING '1' "Proprio
                                       CHANGING c_zlest0146
                                                c_zlest0147
                                                c_zlest0168
                                                c_retorno.
        IF c_retorno-type EQ 'E'.
          RETURN.
        ENDIF.

      ELSE. "NF Terceiro
        PERFORM f_get_documentos_recepcao USING '2' "Terceiro
                                       CHANGING c_zlest0146
                                                c_zlest0147
                                                c_zlest0168
                                                c_retorno.
        IF c_retorno-type EQ 'E'.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM j_1bnfdoc INTO @DATA(_wl_doc)
         WHERE docnum   EQ @c_zlest0147-docnum
           AND candat   EQ @v_candat
           AND cancel   EQ @space.

        IF ( sy-subrc NE 0 ).
          MESSAGE s072 WITH c_zlest0147-docnum INTO v_msg.
          c_retorno-type     = 'E'.
          c_retorno-msgno    = sy-msgno.
          c_retorno-texto    =  v_msg.
          RETURN.
        ENDIF.

        IF c_zlest0147-entrada_propria EQ abap_true. "Definido Documento Fiscal de Entrada Propria para a NF-e
          "Definir Chave NFe Propria
          PERFORM f_atrib_chave_docnum USING _wl_doc
                                    CHANGING v_chave_nfe
                                             v_chave_nff
                                             c_retorno.

          IF c_retorno-type EQ 'E'.
            RETURN.
          ENDIF.

          IF v_chave_nfe IS NOT INITIAL.
            c_zlest0147-chave_nfe_prop = v_chave_nfe.
          ENDIF.
        ENDIF.

      ENDIF.

    WHEN OTHERS.

      CLEAR: c_zlest0147-chave_nfe.

      CLEAR: tg_lfa1[].

      IF c_zlest0147-emissor_cnpj IS NOT INITIAL.
        SELECT *
          FROM lfa1 APPENDING TABLE tg_lfa1
         WHERE stcd1 EQ c_zlest0147-emissor_cnpj.
      ENDIF.

      IF c_zlest0147-emissor_cpf IS NOT INITIAL.
        SELECT *
          FROM lfa1 APPENDING TABLE tg_lfa1
         WHERE stcd2 EQ c_zlest0147-emissor_cpf.
      ENDIF.

      "Elimina Fornecedores Bloqueados
      PERFORM f_elimina_lfa1_bloq TABLES tg_lfa1.

      IF c_zlest0147-emissor_ie IS INITIAL.
        LOOP AT tg_lfa1 WHERE stcd3 IS NOT INITIAL.
          c_zlest0147-emissor_ie = tg_lfa1-stcd3.
          EXIT.
        ENDLOOP.
      ENDIF.

      PERFORM f_get_documentos_recepcao USING '2' "Terceiro
                                     CHANGING c_zlest0146
                                              c_zlest0147
                                              c_zlest0168
                                              c_retorno.
      IF c_retorno-type EQ 'E'.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM j_1bnfdoc INTO _wl_doc
       WHERE docnum   EQ c_zlest0147-docnum
         AND candat   EQ v_candat
         AND cancel   EQ space.

      IF ( sy-subrc NE 0 ).
        MESSAGE s072 WITH c_zlest0147-docnum INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    =  v_msg.
        RETURN.
      ENDIF.

      IF c_zlest0147-entrada_propria EQ abap_true. "Definido Documento Fiscal de Entrada Propria para a NF-f

        "Definir Chave NFe Propria
        PERFORM f_atrib_chave_docnum USING _wl_doc
                                  CHANGING v_chave_nfe
                                           v_chave_nff
                                           c_retorno.

        IF c_retorno-type EQ 'E'.
          RETURN.
        ENDIF.

        IF v_chave_nfe IS NOT INITIAL.
          c_zlest0147-chave_nfe_prop = v_chave_nfe.
        ENDIF.

        "Definir Chave NF-f
        CLEAR: wl_doc_tmp.
        wl_doc_tmp-nfnum   = c_zlest0147-nfnum.
        wl_doc_tmp-model   = c_zlest0147-model.
        wl_doc_tmp-series  = c_zlest0147-serie.
        wl_doc_tmp-docdat  = c_zlest0147-dt_emissao.
        wl_doc_tmp-parid   = _wl_doc-parid.
        wl_doc_tmp-partyp  = _wl_doc-partyp.

        PERFORM f_atrib_chave_j_1bnfdoc USING wl_doc_tmp
                                     CHANGING v_chave_nfe
                                              v_chave_nff
                                              c_retorno.

        IF v_chave_nff IS NOT INITIAL.
          c_zlest0147-chave_nff = v_chave_nff.
        ENDIF.

      ELSE.

        PERFORM f_atrib_chave_docnum USING _wl_doc
                                  CHANGING c_zlest0147-chave_nfe
                                           c_zlest0147-chave_nff
                                           c_retorno.
      ENDIF.

      IF c_retorno-type EQ 'E'.
        RETURN.
      ENDIF.


  ENDCASE.

  IF i_gravar_registro EQ abap_true.

    c_zlest0146-importado                 = 'X'.
    c_zlest0146-dt_importacao             = sy-datum.
    c_zlest0146-hr_importacao             = sy-uzeit.
    c_zlest0146-us_importacao             = sy-uname.

*---------------------------------------------------------------------------------------*
*   Check Duplicidade
*---------------------------------------------------------------------------------------*

    IF c_zlest0147-chave_nfe IS NOT INITIAL.
      CLEAR: tg_0146[].
      SELECT *
        FROM zlest0146 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0146
       WHERE a~cancel  EQ ''
         AND EXISTS ( SELECT *
                        FROM zlest0147 AS b
                       WHERE b~id_recepcao = a~id_recepcao
                         AND b~chave_nfe   = c_zlest0147-chave_nfe ).

      IF tg_0146[] IS NOT INITIAL.
        READ TABLE tg_0146 INDEX 1.
        MESSAGE s101 WITH c_zlest0147-chave_nfe  tg_0146-id_recepcao INTO v_msg.
        c_retorno-type     = 'W'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    =  v_msg.
        RETURN.
      ENDIF.
    ENDIF.

    IF c_zlest0147-chave_nff IS NOT INITIAL.
      CLEAR: tg_0146[].
      SELECT *
        FROM zlest0146 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0146
       WHERE a~cancel  EQ ''
         AND EXISTS ( SELECT *
                        FROM zlest0147 AS b
                       WHERE b~id_recepcao = a~id_recepcao
                         AND b~chave_nff   = c_zlest0147-chave_nff ).

      IF tg_0146[] IS NOT INITIAL.
        READ TABLE tg_0146 INDEX 1.
        MESSAGE s102 WITH c_zlest0147-chave_nff  tg_0146-id_recepcao INTO v_msg.
        c_retorno-type     = 'W'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    =  v_msg.
        RETURN.
      ENDIF.
    ENDIF.

    CLEAR: wl_zlest0146, wl_zlest0147.

    "Cabeçalho Recepção
    IF c_zlest0146-id_recepcao IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZCCT_RCC'
        IMPORTING
          number                  = c_zlest0146-id_recepcao
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF ( sy-subrc NE 0 ) OR ( c_zlest0146-id_recepcao IS INITIAL ).
        MESSAGE s011 WITH 'ZCCT_RCC'INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.
    ENDIF.

    "Nota Fiscal
    c_zlest0147-id_recepcao = c_zlest0146-id_recepcao.

    "Nota Fiscal Rateada
    LOOP AT c_zlest0168 ASSIGNING FIELD-SYMBOL(<fs_0168>).
      <fs_0168>-id_recepcao = c_zlest0146-id_recepcao.
    ENDLOOP.

    CASE c_zlest0147-model.
      WHEN '55'.
        c_zlest0146-tp_recepcao = '1'.  "NF-e
      WHEN OTHERS.
        c_zlest0146-tp_recepcao = '2'.  "NF-f
    ENDCASE.

    MODIFY zlest0146 FROM c_zlest0146.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s013 INTO v_msg.
      c_retorno-type     = 'E'.
      c_retorno-msgno    = sy-msgno.
      c_retorno-texto    = v_msg.
      RETURN.
    ENDIF.

    MODIFY zlest0147 FROM c_zlest0147.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s014 INTO v_msg.
      c_retorno-type     = 'E'.
      c_retorno-msgno    = sy-msgno.
      c_retorno-texto    = v_msg.
      RETURN.
    ENDIF.

    IF c_zlest0168[] IS NOT INITIAL.
      MODIFY zlest0168 FROM TABLE c_zlest0168.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE s103 INTO v_msg.
        c_retorno-type     = 'E'.
        c_retorno-msgno    = sy-msgno.
        c_retorno-texto    = v_msg.
        RETURN.
      ENDIF.
    ENDIF.

    COMMIT WORK.

    MESSAGE s104 WITH c_zlest0146-id_recepcao INTO v_msg.
    c_retorno-type     = 'S'.
    c_retorno-msgno    = sy-msgno.
    c_retorno-texto    = v_msg.
  ELSE.
    IF c_zlest0146-id_recepcao IS NOT INITIAL.
      MESSAGE s105 WITH c_zlest0146-id_recepcao INTO v_msg.
      c_retorno-type     = 'S'.
      c_retorno-msgno    = sy-msgno.
      c_retorno-texto    = v_msg.

      "Nota Fiscal Rateada
      LOOP AT c_zlest0168 ASSIGNING <fs_0168>.
        <fs_0168>-id_recepcao = c_zlest0146-id_recepcao.
      ENDLOOP.
    ENDIF.
  ENDIF.


ENDFUNCTION.
