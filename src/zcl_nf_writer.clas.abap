class ZCL_NF_WRITER definition
  public
  final
  create public .

public section.

  interfaces ZIF_NF_WRITER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NF_WRITER IMPLEMENTATION.


  method ZIF_NF_WRITER~ADD_DOC_REF.

    R_IF_NF_WRITER = ME.

    DATA: WL_ZFIWRT0020 TYPE ZFIWRT0020.

    WL_ZFIWRT0020-DOCNUM = I_DOCNUM.

    APPEND WL_ZFIWRT0020 TO ME->ZIF_NF_WRITER~AT_DOCS_REFERENCIADOS.

  endmethod.


  method ZIF_NF_WRITER~ADD_ITEM.

    R_IF_NF_WRITER = ME.

    APPEND I_ITEM TO ME->ZIF_NF_WRITER~AT_ITENS.

  endmethod.


  method ZIF_NF_WRITER~ADD_PARCEIRO.

    R_IF_NF_WRITER = ME.

    APPEND I_PARCEIRO TO ME->ZIF_NF_WRITER~AT_PARCEIROS.

  endmethod.


  method ZIF_NF_WRITER~CHECK_AUTH_DOC.

    DATA: V_CANDAT_NULL  TYPE J_1BNFDOC-CANDAT.

    CLEAR: R_DOC_AUTH, V_CANDAT_NULL, E_ZFIWRT0008, E_ZFIWRT0009[], E_ACTIVE, E_DOC.

    CHECK I_SEQ_LCTO IS NOT INITIAL.

    SELECT SINGLE *
      FROM ZFIWRT0008 INTO E_ZFIWRT0008
     WHERE SEQ_LCTO EQ I_SEQ_LCTO.

    CHECK SY-SUBRC EQ 0.

    SELECT *
      FROM ZFIWRT0009 INTO TABLE E_ZFIWRT0009
     WHERE SEQ_LCTO EQ I_SEQ_LCTO.

    CHECK SY-SUBRC EQ 0.

    CHECK ( E_ZFIWRT0008-DOCNUM IS NOT INITIAL ).

    SELECT SINGLE *
      FROM J_1BNFE_ACTIVE INTO E_ACTIVE
     WHERE DOCNUM EQ E_ZFIWRT0008-DOCNUM.

    CHECK SY-SUBRC EQ 0.

    SELECT SINGLE *
      FROM J_1BNFDOC INTO E_DOC
     WHERE DOCNUM EQ E_ZFIWRT0008-DOCNUM.

    CHECK SY-SUBRC EQ 0.

    SELECT *
      FROM ZFIWRT0020 INTO TABLE E_ZFIWRT0020
     WHERE SEQ_LCTO EQ I_SEQ_LCTO.

    CHECK ( E_ACTIVE-DOCSTA EQ '1' AND E_ACTIVE-CANCEL EQ ABAP_FALSE AND E_ACTIVE-SCSSTA NE '2' ).

    CHECK ( E_DOC-DOCSTAT EQ '1' AND E_DOC-CANCEL EQ ABAP_FALSE AND E_DOC-CANDAT EQ V_CANDAT_NULL ).

    IF I_CHECK_VALIDADE_DOCUMENTOS EQ ABAP_TRUE.

      IF E_ZFIWRT0008-OBJ_KEY IS NOT INITIAL. "Check Movimento Contabil

        SELECT SINGLE *
          FROM ZFIWRT0011 INTO @DATA(WL_ZFIWRT0011)
         WHERE SEQ_LCTO EQ @I_SEQ_LCTO.

        IF SY-SUBRC EQ 0. "Caso tenha

          SELECT SINGLE *
            FROM ZIB_CONTABIL_CHV INTO @DATA(WL_ZIB_CHV)
           WHERE OBJ_KEY EQ @E_ZFIWRT0008-OBJ_KEY.

          CHECK ( SY-SUBRC EQ 0 ) AND ( WL_ZIB_CHV-BUKRS IS NOT INITIAL ) AND ( WL_ZIB_CHV-BELNR IS NOT INITIAL ).

          SELECT SINGLE *
            FROM BKPF INTO @DATA(WL_BKPF)
           WHERE BUKRS EQ @WL_ZIB_CHV-BUKRS
             AND BELNR EQ @WL_ZIB_CHV-BELNR.

          CHECK ( SY-SUBRC EQ 0 ) AND ( WL_BKPF-STBLG IS INITIAL ).

        ENDIF.

      ENDIF.

    ENDIF.


    R_DOC_AUTH = ABAP_TRUE.

  endmethod.


  method ZIF_NF_WRITER~DETERMINA_FORA_DENTRO_ESTADO.

    R_IF_NF_WRITER = ME.

    CLEAR: E_INDCOPER, E_TEXTO_FISCAL.

    SELECT SINGLE *
      FROM T001W INTO @DATA(WL_T001W)
     WHERE WERKS EQ @ME->ZIF_NF_WRITER~AT_CABECALHO-BRANCH.

    SELECT SINGLE *
      FROM J_1BAD INTO @DATA(WL_J_1BAD)
     WHERE PARVW = @ME->ZIF_NF_WRITER~AT_PARAMETRO_FISCAL-PARVW.

    CHECK SY-SUBRC EQ 0.

    CASE WL_J_1BAD-PARTYP.
      WHEN 'C'.

        SELECT SINGLE *
          FROM KNA1 INTO @DATA(WL_KNA1)
         WHERE KUNNR EQ @ME->ZIF_NF_WRITER~AT_CABECALHO-PARID.

      WHEN 'V' OR 'B'.

        SELECT SINGLE *
          FROM LFA1 INTO @DATA(WL_LFA1)
         WHERE LIFNR EQ @ME->ZIF_NF_WRITER~AT_CABECALHO-PARID.

    ENDCASE.


    CASE WL_J_1BAD-PARTYP.

      WHEN 'C'.
        IF WL_KNA1-REGIO EQ WL_T001W-REGIO.
          E_INDCOPER     = 'D'.
          E_TEXTO_FISCAL = 'Dentro do Estado'.
        ELSE.
          E_INDCOPER = 'F'.
          E_TEXTO_FISCAL = 'Fora do Estado'.
        ENDIF.

        IF ME->ZIF_NF_WRITER~AT_CATEGORIA_NF-DIRECT EQ 1.
          MOVE: WL_KNA1-REGIO TO ME->ZIF_NF_WRITER~AT_SHIPFROM.
        ELSE.
          MOVE: WL_KNA1-REGIO TO ME->ZIF_NF_WRITER~AT_SHIPTO.
        ENDIF.

      WHEN 'V' OR 'B'.

        IF WL_LFA1-REGIO EQ WL_T001W-REGIO.
          E_INDCOPER = 'D'.
          E_TEXTO_FISCAL = 'Dentro do Estado'.
        ELSE.
          E_INDCOPER = 'F'.
          E_TEXTO_FISCAL = 'Fora do Estado'.
        ENDIF.

        IF ME->ZIF_NF_WRITER~AT_CATEGORIA_NF-DIRECT EQ 1.
          MOVE: WL_LFA1-REGIO TO ME->ZIF_NF_WRITER~AT_SHIPFROM.
        ELSE.
          MOVE: WL_LFA1-REGIO TO ME->ZIF_NF_WRITER~AT_SHIPTO.
        ENDIF.

    ENDCASE.

    IF ME->ZIF_NF_WRITER~AT_CATEGORIA_NF-DIRECT EQ '1'.
      MOVE: WL_T001W-REGIO TO ME->ZIF_NF_WRITER~AT_SHIPTO.
    ELSE.
      MOVE: WL_T001W-REGIO TO ME->ZIF_NF_WRITER~AT_SHIPFROM.
    ENDIF.

  endmethod.


  method ZIF_NF_WRITER~ESTORNAR_DOCUMENTO.

  DATA: T_DOCS TYPE TABLE OF ZFIWRS0003.

  DATA: I_TIMES_WAIT TYPE I.

  CLEAR: R_ESTORNADO.

  IF ( I_SEQ_LCTO IS INITIAL ).
    RAISE EXCEPTION TYPE ZCX_NF_WRITER
       EXPORTING
         TEXTID = VALUE #( MSGID = ZCX_NF_WRITER=>ZCX_DATA_NOT_INFORMED-MSGID
                           MSGNO = ZCX_NF_WRITER=>ZCX_DATA_NOT_INFORMED-MSGNO
                           ATTR1 = CONV #( 'Seq. Lcto' )
                           )
         MSGTY  = 'E'
         MSGNO  = ZCX_NF_WRITER=>ZCX_DATA_NOT_INFORMED-MSGNO
         MSGID  = ZCX_NF_WRITER=>ZCX_DATA_NOT_INFORMED-MSGID
         MSGV1   = CONV #( 'Seq. Lcto' ).
  ENDIF.

  SELECT SINGLE *
    FROM ZFIWRT0008 INTO @DATA(WL_ZFIWRT0008)
   WHERE SEQ_LCTO EQ @I_SEQ_LCTO.

  IF SY-SUBRC NE 0.
    RAISE EXCEPTION TYPE ZCX_NF_WRITER
       EXPORTING
         TEXTID = VALUE #( MSGID = ZCX_NF_WRITER=>ZCX_DATA_NOT_INFORMED-MSGID
                           MSGNO = ZCX_NF_WRITER=>ZCX_DATA_NOT_INFORMED-MSGNO
                           ATTR1 = CONV #( |Lcto ZNFW: { I_SEQ_LCTO }| )
                           )
         MSGTY  = 'E'
         MSGNO  = ZCX_NF_WRITER=>ZCX_DATA_NOT_INFORMED-MSGNO
         MSGID  = ZCX_NF_WRITER=>ZCX_DATA_NOT_INFORMED-MSGID
         MSGV1   = CONV #( |Lcto ZNFW: { I_SEQ_LCTO }| ).
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       PERCENTAGE = SY-TABIX
       TEXT       = |Estornando Seq.Lcto: { I_SEQ_LCTO } ...|.

  IF WL_ZFIWRT0008-DOCS_ESTORNADOS EQ ABAP_TRUE.
    MESSAGE S009(ZNFW).
    R_ESTORNADO = ABAP_TRUE.
    RETURN.
  ENDIF.

*-----------------------------------------------------------------------------------------------------------------*
* Checar se deve marcar registro como eliminado
*-----------------------------------------------------------------------------------------------------------------*
  IF ( WL_ZFIWRT0008-DOCNUM IS INITIAL    ) AND  "Não pode ter gerado doc. fiscal
     ( WL_ZFIWRT0008-MBLNR  IS INITIAL    ) AND  "Não pode ter gerado doc. material
     ( WL_ZFIWRT0008-STATUS IS INITIAL OR        "Não pode esta em processamento
       WL_ZFIWRT0008-STATUS EQ 'E'        ) .

    DATA(_GEROU_DOC_CONTABIL) = ABAP_FALSE.
    IF WL_ZFIWRT0008-OBJ_KEY IS NOT INITIAL. "Check Movimento Contabil

      SELECT SINGLE *
        FROM ZFIWRT0011 INTO @DATA(WL_ZFIWRT0011)
       WHERE SEQ_LCTO EQ @WL_ZFIWRT0008-SEQ_LCTO.

      IF SY-SUBRC EQ 0. "Caso tenha
        SELECT SINGLE *
          FROM ZIB_CONTABIL_CHV INTO @DATA(WL_ZIB_CHV)
         WHERE OBJ_KEY EQ @WL_ZFIWRT0008-OBJ_KEY.

        IF ( SY-SUBRC EQ 0 ) AND ( WL_ZIB_CHV-BELNR IS NOT INITIAL ).
          _GEROU_DOC_CONTABIL = ABAP_TRUE.
        ENDIF.
      ENDIF.
    ENDIF.

    IF _GEROU_DOC_CONTABIL EQ ABAP_FALSE. "Não pode ter gerado doc. contabil
      UPDATE ZFIWRT0008 SET LOEKZ = ABAP_TRUE
       WHERE SEQ_LCTO = WL_ZFIWRT0008-SEQ_LCTO.

      MESSAGE S009(ZNFW).
      R_ESTORNADO = ABAP_TRUE.
      RETURN.
    ENDIF.

  ENDIF.


  IF I_WAIT_ESTORNO EQ ABAP_TRUE.
    I_TIMES_WAIT = 240.
  ELSE.
    I_TIMES_WAIT = 1.
  ENDIF.

  DO I_TIMES_WAIT TIMES.

    CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
      EXPORTING
        I_SEQ_LCTO  = I_SEQ_LCTO
        I_ESTORNO   = ABAP_TRUE
      TABLES
        T_DOCS      = T_DOCS.

    SELECT SINGLE *
      FROM ZFIWRT0008 INTO @DATA(WL_0008_CHECK_ESTORNO)
     WHERE SEQ_LCTO EQ @I_SEQ_LCTO.

    IF ( SY-SUBRC = 0 ) AND ( WL_0008_CHECK_ESTORNO-DOCS_ESTORNADOS EQ ABAP_TRUE ).

      COMMIT WORK AND WAIT .

      MESSAGE S009(ZNFW).
      R_ESTORNADO = ABAP_TRUE.

      RETURN.

    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = SY-TABIX
        TEXT       = |Estornando Seq.Lcto: { I_SEQ_LCTO }... Time: { SY-INDEX }... |.

    WAIT UP TO 5 SECONDS.

  ENDDO.



  endmethod.


  method ZIF_NF_WRITER~GET_INSTANCE.

    IF ZIF_NF_WRITER~AT_IF_NF_WRITER IS NOT BOUND.
      CREATE OBJECT ZIF_NF_WRITER~AT_IF_NF_WRITER TYPE ZCL_NF_WRITER.
    ENDIF.

    R_IF_NF_WRITER = ZIF_NF_WRITER~AT_IF_NF_WRITER.

  endmethod.


  METHOD zif_nf_writer~get_monta_contabil.

    r_if_nf_writer = me.

    e_contabil[] =  me->zif_nf_writer~at_contabil[].

  ENDMETHOD.


  method zif_nf_writer~gravar_documento.

    r_if_nf_writer = me.

*-CS2020001225 - 20.09.2021 - JT - inicio
    if i_nao_valida = abap_false.
      me->zif_nf_writer~validar_registro( ).
    endif.
*-CS2020001225 - 20.09.2021 - JT - fim

*-CS2020001225 - 20.09.2021 - JT - inicio
    if i_nao_prepara = abap_false.
      me->zif_nf_writer~prepara_lancamento( ).
    endif.
*-CS2020001225 - 20.09.2021 - JT - fim

    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = '1'
        object                  = 'ZSEQ_LCTO'
      importing
        number                  = me->zif_nf_writer~at_cabecalho-seq_lcto
      exceptions
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        others                  = 8.

    if ( sy-subrc ne 0 ) and ( me->zif_nf_writer~at_cabecalho-seq_lcto is initial ).
      raise exception type zcx_nf_writer
        exporting
          textid = value #( msgid = zcx_nf_writer=>zcx_objeto_num_not_found-msgid
                            msgno = zcx_nf_writer=>zcx_objeto_num_not_found-msgno
                            attr1 = conv #( '1' )
                            attr2 = conv #( 'ZSEQ_LCTO' )
                            )
          msgty  = 'E'
          msgno  = zcx_nf_writer=>zcx_objeto_num_not_found-msgno
          msgid  = zcx_nf_writer=>zcx_objeto_num_not_found-msgid
          msgv1  = conv #( '1' )
          msgv2  = conv #( 'ZSEQ_LCTO' ).
    endif.

    modify zfiwrt0008 from me->zif_nf_writer~at_cabecalho.

*--------------------------------------------------------------------------------------------*
*   Dados Transp
*--------------------------------------------------------------------------------------------*
    if me->zif_nf_writer~at_dados_transp is not initial.
      zif_nf_writer~at_dados_transp-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
      modify zfiwrt0019 from me->zif_nf_writer~at_dados_transp.
    endif.
*--------------------------------------------------------------------------------------------*
*   Itens
*--------------------------------------------------------------------------------------------*
    loop at me->zif_nf_writer~at_itens assigning field-symbol(<fs_item>).
      <fs_item>-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
    endloop.

    modify zfiwrt0009 from table me->zif_nf_writer~at_itens.

*--------------------------------------------------------------------------------------------*
*    Impostos
*--------------------------------------------------------------------------------------------*
    if me->zif_nf_writer~at_impostos[] is not initial.

      loop at me->zif_nf_writer~at_impostos assigning field-symbol(<fs_imposto>).
        <fs_imposto>-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
        if i_nao_imposto = abap_true."107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA
          <fs_imposto>-taxval = 0.
          <fs_imposto>-othbas = 0.
          <fs_imposto>-excbas = 0.
        endif.
      endloop.

      modify zfiwrt0010 from table me->zif_nf_writer~at_impostos.
    endif.

*--------------------------------------------------------------------------------------------*
*    Rateio
*--------------------------------------------------------------------------------------------*

    if me->zif_nf_writer~at_rateio[] is not initial.

      loop at me->zif_nf_writer~at_rateio assigning field-symbol(<fs_rateio>).
        <fs_rateio>-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
      endloop.

      modify zfiwrt0023 from table me->zif_nf_writer~at_rateio.
    endif.

*--------------------------------------------------------------------------------------------*
*    Contabil
*--------------------------------------------------------------------------------------------*
    if me->zif_nf_writer~at_contabil[] is not initial.

      loop at me->zif_nf_writer~at_contabil assigning field-symbol(<fs_contabil>).
        <fs_contabil>-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
      endloop.

      modify zfiwrt0011 from table me->zif_nf_writer~at_contabil.

    endif.

*--------------------------------------------------------------------------------------------*
*   Mov. Estoque
*--------------------------------------------------------------------------------------------*
    if me->zif_nf_writer~at_mov_estoque[] is not initial.

      loop at me->zif_nf_writer~at_mov_estoque assigning field-symbol(<fs_mov_estoque>).
        <fs_mov_estoque>-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
      endloop.

      modify zfiwrt0012 from table me->zif_nf_writer~at_mov_estoque.
    endif.

*--------------------------------------------------------------------------------------------*
*   Mensagens
*--------------------------------------------------------------------------------------------*
    if me->zif_nf_writer~at_mensagens[] is not initial.

      loop at me->zif_nf_writer~at_mensagens assigning field-symbol(<fs_mensagem>).
        <fs_mensagem>-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
      endloop.

      modify zfiwrt0013 from table me->zif_nf_writer~at_mensagens.
    endif.

*--------------------------------------------------------------------------------------------*
*   Parceiros
*--------------------------------------------------------------------------------------------*
    if me->zif_nf_writer~at_parceiros[] is not initial.

      loop at me->zif_nf_writer~at_parceiros assigning field-symbol(<fs_parceiro>).
        <fs_parceiro>-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
      endloop.

      modify zfiwrt0015 from table me->zif_nf_writer~at_parceiros.
    endif.

*--------------------------------------------------------------------------------------------*
*   Faturamento energia
*--------------------------------------------------------------------------------------------*
    if me->zif_nf_writer~at_fatura is not initial.
      zif_nf_writer~at_fatura-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
      modify zfiwrt0022 from me->zif_nf_writer~at_fatura.
    endif.

*--------------------------------------------------------------------------------------------*
*    Docs. Referenciados
*--------------------------------------------------------------------------------------------*
    if me->zif_nf_writer~at_docs_referenciados[] is not initial.

      loop at me->zif_nf_writer~at_docs_referenciados assigning field-symbol(<fs_docref>).
        <fs_docref>-seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.
      endloop.

      modify zfiwrt0020 from table me->zif_nf_writer~at_docs_referenciados.

    endif.

    commit work.

    e_seq_lcto = me->zif_nf_writer~at_cabecalho-seq_lcto.

    if i_processar_lcto eq abap_true.
      call function 'ZNFW_PROCESSA_SEQ_LCTO'
        exporting
          i_seq_lcto = e_seq_lcto.
    endif.

  endmethod.


  METHOD zif_nf_writer~monta_contabil.

    r_if_nf_writer = me.

    DATA: tl_1baj  TYPE TABLE OF j_1baj,
          tl_1bajt TYPE TABLE OF j_1bajt,
          tl_tbsl  TYPE TABLE OF tbsl,
          tl_skat  TYPE TABLE OF skat,
          tl_cskb  TYPE TABLE OF cskb,
          tl_user  TYPE TABLE OF user_addr.

    DATA: tl_impo        TYPE zfiwrt0010_t,
          wl_impo        TYPE zfiwrt0010,
          wl_contab      TYPE zfiwrt0011,
          wl_zfiwrt0011  TYPE zfiwrt0011,
          wa_tka02       TYPE tka02,
          v_count        TYPE i,
          v_bseg         TYPE c,
          t_hkont        TYPE STANDARD TABLE OF rgsb4,
          v_parid        TYPE zfiwrt0008-parid,
          v_koart        TYPE tbsl-koart,
          v_vbund        TYPE bseg-vbund,
          e_contabil_aux TYPE zfiwrt0011_t,
          v_code         TYPE zfiwrt0008-tcode_org VALUE 'ZSDT0165',
          vlr_br         TYPE zfiwrt0011-dmbtr,
          v_dmbtr        TYPE zfiwrt0011-dmbtr.

    DATA: lv_controllingarea TYPE  bapi1030_gen-co_area,
          lv_costelement     TYPE  bapi1030_gen-cost_elem,
          lv_keydate         TYPE  bapi1030_gen-some_date,
          lt_returns         TYPE TABLE OF bapiret2,
          ls_coeldes         TYPE bapi1030_ceoutputlist.


    CLEAR: e_contabil[], tl_impo, v_koart, tl_impo[].

*-CS2020001225 - 20.09.2021 - JT - inicio
    IF i_contabil[] IS NOT INITIAL.
      e_contabil[] = i_contabil[].
      EXIT.
    ENDIF.
*-CS2020001225 - 20.09.2021 - JT - fim

    IF me->zif_nf_writer~at_parametros_contabil[] IS NOT INITIAL.

      SELECT *
        FROM tbsl INTO TABLE tl_tbsl
         FOR ALL ENTRIES IN me->zif_nf_writer~at_parametros_contabil
       WHERE bschl EQ me->zif_nf_writer~at_parametros_contabil-bschl.

      SELECT *
        FROM skat INTO TABLE tl_skat
         FOR ALL ENTRIES IN me->zif_nf_writer~at_parametros_contabil
       WHERE spras EQ sy-langu
         AND ktopl EQ '0050'
         AND saknr EQ me->zif_nf_writer~at_parametros_contabil-hkont.

* ---> S4 Migration - 17/07/2023 - CA
*      SELECT *
*        FROM cskb INTO TABLE tl_cskb
*         FOR ALL ENTRIES IN me->zif_nf_writer~at_parametros_contabil
*       WHERE kstar EQ me->zif_nf_writer~at_parametros_contabil-hkont
*         AND ( datbi GE sy-datum AND datab LE sy-datum )
*         AND katyp EQ '01'.
* <--- S4 Migration - 17/07/2023 - CA

    ENDIF.

    LOOP AT me->zif_nf_writer~at_parametros_contabil INTO DATA(wl_contabil_parametro).

      CLEAR: wl_zfiwrt0011.

      MOVE-CORRESPONDING wl_contabil_parametro TO wl_zfiwrt0011.

      APPEND wl_zfiwrt0011 TO e_contabil.
    ENDLOOP.

    CHECK e_contabil[] IS NOT INITIAL.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class         = '0000'
        setnr         = 'CONTAS_EC-CS'
      TABLES
        set_values    = t_hkont
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.


    SELECT SINGLE *
      FROM tka02 INTO wa_tka02
     WHERE bukrs = me->zif_nf_writer~at_cabecalho-bukrs.

    CONCATENATE 'CE4' wa_tka02-kokrs '_ACCT' INTO DATA(tabco1).
    CONCATENATE 'CE4' wa_tka02-kokrs         INTO DATA(tabco2).

    LOOP AT e_contabil ASSIGNING FIELD-SYMBOL(<fs_contabil>).

      READ TABLE tl_tbsl INTO DATA(wl_tbsl) WITH KEY bschl = <fs_contabil>-bschl.
      IF wl_tbsl-koart = 'D' OR wl_tbsl-koart = 'K'.
        v_koart = wl_tbsl-koart.
      ENDIF.

      <fs_contabil>-dmbtr = 0.
    ENDLOOP.

    LOOP AT me->zif_nf_writer~at_itens INTO DATA(wl_item).

*-CS2020001225 - 20.09.2021 - JT - inicio
      "Calcular Impostos Item
      me->zif_nf_writer~monta_impostos( EXPORTING i_impostos = i_impostos[]
                                        IMPORTING e_impostos = DATA(t_impostos_item)
                                        CHANGING  c_item     = wl_item ).
*-CS2020001225 - 20.09.2021 - JT - fim

      LOOP AT t_impostos_item INTO DATA(wl_imposto_item).
        COLLECT wl_imposto_item INTO tl_impo.
      ENDLOOP.

    ENDLOOP.

    LOOP AT e_contabil ASSIGNING FIELD-SYMBOL(<fs_contabil_item>).

      CLEAR: wl_impo, wl_tbsl.

      READ TABLE tl_tbsl INTO wl_tbsl WITH KEY bschl = <fs_contabil_item>-bschl.

      "Sociedade Parceira
      CLEAR: <fs_contabil_item>-vbund, v_vbund.

      READ TABLE t_hkont INTO DATA(wl_hkont) WITH KEY from = <fs_contabil_item>-hkont.

      IF sy-subrc = 0.

        IF v_koart = 'D'.
          SELECT SINGLE vbund INTO v_vbund FROM kna1
            WHERE kunnr = me->zif_nf_writer~at_cabecalho-parid "soc parceira do emissor
            AND   ktokd IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ').
        ELSEIF v_koart = 'K'.
          SELECT SINGLE vbund INTO v_vbund FROM lfa1
            WHERE lifnr = me->zif_nf_writer~at_cabecalho-parid "soc parceira do emissor
            AND   ktokk IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ').
        ENDIF.
*
*        IF v_koart = 'D' OR v_koart = 'K'.
*          <fs_contabil_item>-hkont = me->zif_nf_writer~at_cabecalho-parid.
*        ENDIF.


        <fs_contabil_item>-vbund = v_vbund.
      ENDIF.

      IF <fs_contabil_item>-taxtyp IS INITIAL.
        IF me->zif_nf_writer~at_parametro_fiscal-complemento = 'S'.
          LOOP AT tl_impo INTO wl_impo
            WHERE taxtyp EQ 'ICM3'.
            IF wl_tbsl-shkzg EQ 'H'.
              SUBTRACT  wl_impo-taxval FROM <fs_contabil_item>-dmbtr.
            ELSE.
              ADD wl_impo-taxval TO <fs_contabil_item>-dmbtr.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF me->zif_nf_writer~at_parametro_fiscal-energia EQ 'N' .
          LOOP AT me->zif_nf_writer~at_itens INTO wl_item.
            IF wl_tbsl-shkzg EQ 'H'.
              SUBTRACT  wl_item-netwr FROM <fs_contabil_item>-dmbtr.
            ELSE.
              ADD wl_item-netwr TO <fs_contabil_item>-dmbtr.
            ENDIF.
          ENDLOOP.
        ELSEIF me->zif_nf_writer~at_parametro_fiscal-energia EQ 'S'.
          LOOP AT tl_impo INTO wl_impo
            WHERE taxtyp EQ 'ICS1'.
            IF wl_tbsl-shkzg EQ 'H'.
              CLEAR: v_dmbtr.
              v_dmbtr = ( wl_item-netwr + wl_impo-taxval ). "Valor total do item + imposto
              SUBTRACT v_dmbtr FROM <fs_contabil_item>-dmbtr.
*            SUBTRACT  wl_impo-base FROM <fs_contabil_item>-dmbtr.
            ELSE.
              CLEAR: v_dmbtr.
              v_dmbtr = ( wl_item-netwr + wl_impo-taxval ). "Valor total do item + imposto
              ADD v_dmbtr TO <fs_contabil_item>-dmbtr.
*              ADD wl_impo-base TO <fs_contabil_item>-dmbtr.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.
        READ TABLE tl_impo INTO wl_impo WITH KEY taxtyp = <fs_contabil_item>-taxtyp.
        IF sy-subrc IS INITIAL.
          IF wl_tbsl-shkzg EQ 'H'.
            MOVE: wl_impo-taxval TO <fs_contabil_item>-dmbtr.
            MULTIPLY <fs_contabil_item>-dmbtr BY -1.
          ELSE.
            MOVE: wl_impo-taxval TO <fs_contabil_item>-dmbtr.
          ENDIF.
        ENDIF.
      ENDIF.

      IF me->zif_nf_writer~at_cabecalho-tcode_org = v_code.
        SELECT SINGLE *
          FROM  zsdt0225 INTO @DATA(wa_0225)
         WHERE id_seq EQ @me->zif_nf_writer~at_cabecalho-ch_referencia.

        IF sy-subrc = 0.
          CLEAR vlr_br.

          vlr_br = <fs_contabil_item>-dmbtr.
          <fs_contabil_item>-curha = 'USD'.
          <fs_contabil_item>-dmbe2 = ( vlr_br / wa_0225-tax_dolar ).
        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR v_bseg.

    IF me->zif_nf_writer~at_parametros_contabil[] IS INITIAL.

      LOOP AT me->zif_nf_writer~at_docs_referenciados INTO DATA(wa_docrefs).

        SELECT * INTO TABLE @DATA(itens_documento)
          FROM j_1bnflin
         WHERE docnum EQ @wa_docrefs-docnum.

        SELECT SINGLE parid
          FROM j_1bnfdoc
          INTO v_parid
          WHERE docnum EQ wa_docrefs-docnum.

        LOOP AT itens_documento INTO DATA(wa_itens_documento).

          CHECK wa_itens_documento-refkey IS NOT INITIAL.

          CONCATENATE wa_itens_documento-refkey '%' INTO wa_itens_documento-refkey.

          SELECT * INTO TABLE @DATA(it_bkpf)
            FROM bkpf
           WHERE awkey LIKE @wa_itens_documento-refkey.

          IF sy-subrc IS INITIAL.
* ---> S4 Migration - 15/06/2023 - MA
*            SELECT * INTO TABLE @DATA(it_bseg)
*              FROM bseg
*               FOR ALL ENTRIES IN @it_bkpf
*             WHERE bukrs EQ @it_bkpf-bukrs
*               AND belnr EQ @it_bkpf-belnr
*               AND gjahr EQ @it_bkpf-gjahr.


            DATA lt_fields TYPE fagl_t_field.
            DATA: lt_bseg TYPE TABLE OF bseg,
                  it_bseg TYPE TABLE OF bseg.

*          LT_FIELDS = value #( ( LINE = 'BUKRS' )
*                               ( LINE = 'BELNR' )
*                               ( LINE = 'GJAHR' )
*                               ).

            CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
              EXPORTING
                it_for_all_entries = it_bkpf
                i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
*               IT_FIELDLIST       = LT_FIELDS
              IMPORTING
                et_bseg            = lt_bseg
              EXCEPTIONS
                not_found          = 1.

            IF sy-subrc = 0 AND lines( lt_bseg ) > 0.
              APPEND LINES OF lt_bseg TO it_bseg.
              sy-dbcnt = lines( lt_bseg ).
            ELSE.
              sy-subrc = 4.
              sy-dbcnt = 0.
            ENDIF.
* <--- S4 Migration - 15/06/2023 - MA

            SELECT * INTO TABLE @DATA(it_tbsl)
              FROM tbsl
               FOR ALL ENTRIES IN @it_bseg
             WHERE bschl EQ @it_bseg-bschl.

            SORT it_tbsl BY bschl.
            CLEAR v_koart.
            LOOP AT it_bseg INTO DATA(wa_bseg2).
              READ TABLE it_tbsl INTO DATA(wa_tbsl2) WITH KEY bschl = wa_bseg2-bschl BINARY SEARCH.
              IF wa_tbsl2-koart = 'D' OR wa_tbsl2-koart = 'K'.
                v_koart = wa_tbsl2-koart.
              ENDIF.
            ENDLOOP.

            LOOP AT it_bseg INTO DATA(wa_bseg).
              v_bseg = 'X'.
              CLEAR: wl_contab.
              READ TABLE it_bkpf INTO DATA(wa_bkpf) WITH KEY bukrs = wa_bseg-bukrs
                                                             belnr = wa_bseg-belnr
                                                             gjahr = wa_bseg-gjahr.
              wl_contab-waers   = wa_bkpf-waers.
              wl_contab-waers_i = wa_bkpf-hwaer.
              wl_contab-curha	  = wa_bkpf-hwae2.
              wl_contab-curin	  = wa_bkpf-hwae3.
              READ TABLE it_tbsl INTO DATA(wa_tbsl) WITH KEY bschl = wa_bseg-bschl BINARY SEARCH.

              CASE wa_tbsl-koart.
                WHEN 'D'.
                  wl_contab-hkont  = wa_bseg-kunnr.
                WHEN 'K'.
                  wl_contab-hkont  = wa_bseg-lifnr.
                WHEN OTHERS.
                  wl_contab-hkont  = wa_bseg-hkont.
              ENDCASE.

              wl_contab-bschl  = wa_tbsl-stbsl.

              IF wa_tbsl-shkzg EQ 'H'.
                ADD wa_bseg-dmbtr TO wl_contab-dmbtr.
                wl_contab-dmbtr = wl_contab-dmbtr * -1.

                ADD wa_bseg-dmbe2 TO wl_contab-dmbe2.
                MULTIPLY wl_contab-dmbe2 BY -1.

                ADD wa_bseg-dmbe3 TO wl_contab-dmbe3.
                MULTIPLY wl_contab-dmbe3 BY -1.

                ADD wa_bseg-wrbtr TO wl_contab-wrbtr.
                MULTIPLY wl_contab-wrbtr BY -1.
              ELSE.
                ADD wa_bseg-dmbtr TO wl_contab-dmbtr.
                ADD wa_bseg-dmbe2 TO wl_contab-dmbe2.
                ADD wa_bseg-dmbe3 TO wl_contab-dmbe3.
                ADD wa_bseg-wrbtr TO wl_contab-wrbtr.
              ENDIF.

              CLEAR wl_contab-artnr.
              IF wa_bseg-paobjnr IS NOT INITIAL AND wa_bseg-paobjnr GT 0.
                CLEAR v_count.
                SELECT COUNT(*) FROM  dd02l
                    INTO    v_count
                    WHERE   tabname = tabco1
                    AND     as4local  = 'A'.
                IF v_count GT 0.
                  SELECT SINGLE artnr
                    FROM (tabco1)
                    INTO wl_contab-artnr
                  WHERE paobjnr = wa_bseg-paobjnr.
                ENDIF.

                IF sy-subrc NE 0 OR v_count = 0.
                  CLEAR  v_count.
                  SELECT COUNT(*) FROM  dd02l
                    INTO    v_count
                    WHERE   tabname = tabco2
                    AND     as4local  = 'A'.
                  IF v_count GT 0.
                    SELECT SINGLE artnr
                      FROM (tabco2)
                      INTO wl_contab-artnr
                    WHERE paobjnr = wa_bseg-paobjnr.
                  ENDIF.
                ENDIF.
              ENDIF.

              wl_contab-taxtyp  = wa_bseg-taxps.
              wl_contab-zfbdt   = wa_bseg-zfbdt.
              wl_contab-kostl   = wa_bseg-kostl.
              wl_contab-umskz   = wa_bseg-umsks.

              "Sociedade Parceira
              CLEAR: <fs_contabil_item>-vbund, v_vbund.
              READ TABLE t_hkont INTO wl_hkont WITH KEY from = wl_contab-hkont.
              IF sy-subrc = 0.
                IF v_koart = 'D'.
                  SELECT SINGLE vbund INTO v_vbund FROM kna1
                    WHERE kunnr = v_parid "soc parceira do emissor
                    AND   ktokd IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ').
                  wl_contab-vbund = v_vbund.
                ELSEIF v_koart = 'K'.
                  SELECT SINGLE vbund INTO v_vbund FROM lfa1
                    WHERE lifnr = v_parid "soc parceira do emissor
                    AND   ktokk IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ').
                  wl_contab-vbund = v_vbund.
                ENDIF.
              ENDIF.

              APPEND wl_contab TO e_contabil.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ENDIF.

    IF v_bseg = 'X'.
      DELETE e_contabil WHERE dmbtr = 0.
    ENDIF.

    LOOP AT e_contabil INTO DATA(wa_contabil).
* ---> S4 Migration - 17/07/2023 - CA
*      READ TABLE tl_cskb  WITH KEY kstar = wa_contabil-hkont TRANSPORTING NO FIELDS.
*      IF sy-subrc IS INITIAL.
*        APPEND wa_contabil TO e_contabil_aux.
*      ENDIF.

      LOOP AT zif_nf_writer~at_parametros_contabil INTO DATA(ls_param_contabil).

        lv_controllingarea  = wa_tka02-kokrs.
        lv_costelement      = ls_param_contabil-hkont.
        lv_keydate          = sy-datum.

        CLEAR: lt_returns[], ls_coeldes.

        CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
          EXPORTING
            controllingarea   = lv_controllingarea
            costelement       = lv_costelement
            keydate           = lv_keydate
          IMPORTING
            costelementdetail = ls_coeldes
          TABLES
            return            = lt_returns.

        READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc IS NOT INITIAL AND
           ls_coeldes-celem_category = '01'. "Critério de seleção

          APPEND wa_contabil TO e_contabil_aux.
        ENDIF.

        CLEAR: ls_param_contabil, lv_costelement.
      ENDLOOP.
* <--- S4 Migration - 17/07/2023 - CA
    ENDLOOP.

    LOOP AT e_contabil_aux INTO wa_contabil.
      DELETE e_contabil WHERE hkont EQ wa_contabil-hkont.
    ENDLOOP.

    LOOP AT  me->zif_nf_writer~at_rateio ASSIGNING FIELD-SYMBOL(<wa_rateio>).
      LOOP AT e_contabil_aux INTO wa_contabil.
        wa_contabil-dmbtr = wa_contabil-dmbtr * ( <wa_rateio>-perc / 100 ).
        wa_contabil-dmbe2 = wa_contabil-dmbe2 * ( <wa_rateio>-perc / 100 ).
        wa_contabil-dmbe3 = wa_contabil-dmbe3 * ( <wa_rateio>-perc / 100 ).
        wa_contabil-kostl = <wa_rateio>-kostl.
        APPEND wa_contabil TO e_contabil.
      ENDLOOP.
      <wa_rateio>-bschl =  wa_contabil-bschl.
    ENDLOOP.

    IF me->zif_nf_writer~at_cabecalho-zlsch IS NOT INITIAL.
      LOOP AT e_contabil ASSIGNING <fs_contabil>.
        READ TABLE tl_tbsl INTO wl_tbsl WITH KEY bschl = <fs_contabil>-bschl.
        IF wl_tbsl-koart = 'D' OR wl_tbsl-koart = 'K'.
          <fs_contabil>-hkont = me->zif_nf_writer~at_cabecalho-PARID.
          <fs_contabil>-zlsch = me->zif_nf_writer~at_cabecalho-zlsch.
          <fs_contabil>-zfbdt = me->zif_nf_writer~at_cabecalho-zfbdt.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT e_contabil BY taxtyp bschl.

  ENDMETHOD.


  METHOD zif_nf_writer~monta_impostos.

    r_if_nf_writer = me.

    CONSTANTS: c_icm3(4) TYPE c VALUE 'ICM3',
               c_ipis(4) TYPE c VALUE 'IPIS',
               c_icof(4) TYPE c VALUE 'ICOF',
               c_ics1(4) TYPE c VALUE 'ICS1',
               c_0       TYPE c VALUE '0',
               c_1       TYPE c VALUE '1',
               c_2       TYPE c VALUE '2',
               c_b       TYPE c VALUE 'B',
               c_s       TYPE c VALUE 'S',
               c_l       TYPE c VALUE 'L',
               c_x       TYPE c VALUE 'X',
               c_d       TYPE c VALUE 'D',
               c_k       TYPE c VALUE 'K',
               c_w       TYPE c VALUE 'W',
               c_f       TYPE c VALUE 'F',
               c_t       TYPE c VALUE 'T',
               c_i       TYPE c VALUE 'I',
               c_n       TYPE c VALUE 'N',
               c_h       TYPE c VALUE 'H',
               c_01(2)   TYPE c VALUE '01',
               c_30(2)   TYPE c VALUE '30',
               c_40(2)   TYPE c VALUE '40',
               c_50(4)   TYPE c VALUE '0050',
               c_76(2)   TYPE c VALUE '76',
               c_71(2)   TYPE c VALUE '71',
               c_72(2)   TYPE c VALUE '72',
               c_br(2)   TYPE c VALUE 'BR',
               c_ag(2)   TYPE c VALUE 'AG',
               c_lf(2)   TYPE c VALUE 'LF',
               c_lr(2)   TYPE c VALUE 'LR',
               c_z1(2)   TYPE c VALUE 'Z1'.

    DATA: tl_1baj  TYPE TABLE OF j_1baj,
          tl_1bajt TYPE TABLE OF j_1bajt,
          tl_tbsl  TYPE TABLE OF tbsl,
          tl_skat  TYPE TABLE OF skat,
          tl_cskb  TYPE TABLE OF cskb,
          tl_user  TYPE TABLE OF user_addr.

    DATA: BEGIN OF wl_1btxic,
            rate TYPE j_1btxic3-rate,
            base TYPE j_1btxic3-base,
          END OF wl_1btxic.

    DATA: wl_base_aux TYPE j_1btxic3-base,
          wl_a924     TYPE a924,
          wl_konp     TYPE konp,
          wl_t001w    TYPE t001w,
          wl_1btxpis  TYPE j_1btxpis,
          wl_1btxcof  TYPE j_1btxcof,
          wl_imposto  TYPE zfiwrt0010.

    CLEAR: wl_a924, wl_konp, wl_t001w, e_impostos[].

    DATA: v_line TYPE i,
          v_ics1 TYPE zfiwrt0010-taxval.
*
    v_ics1 = 0.
    v_line = 1.

*-CS2020001225 - 20.09.2021 - JT - inicio
    IF i_impostos[] IS NOT INITIAL.
      e_impostos[] = i_impostos[].
      EXIT.
    ENDIF.
*-CS2020001225 - 20.09.2021 - JT - fim

*-ir147555 - 20.09.2023 - lf - inicio
    IF me->zif_nf_writer~at_parametros_regulatorios[] IS INITIAL.
      me->zif_nf_writer~set_parametro_regulatorio( ).
    ENDIF.
*-IR147555 - 20.09.2023 - LF - fim



    IF me->zif_nf_writer~at_parametros_impostos[] IS NOT INITIAL.

      SELECT *
        FROM j_1baj INTO TABLE tl_1baj
         FOR ALL ENTRIES IN me->zif_nf_writer~at_parametros_impostos
       WHERE taxtyp EQ me->zif_nf_writer~at_parametros_impostos-taxtyp.

      SELECT *
        FROM j_1bajt INTO TABLE tl_1bajt
         FOR ALL ENTRIES IN me->zif_nf_writer~at_parametros_impostos
       WHERE spras  EQ sy-langu
         AND taxtyp EQ me->zif_nf_writer~at_parametros_impostos-taxtyp.

    ENDIF.

    SELECT SINGLE *
      FROM j_1btxsdc INTO @DATA(wl_1btxsdc)
     WHERE taxcode EQ @me->zif_nf_writer~at_leis_estado-taxcode.

    CASE me->zif_nf_writer~at_categoria_nf-direct.
      WHEN '1'. "Entradas

        LOOP AT me->zif_nf_writer~at_parametros_impostos INTO DATA(wl_imposto_operacao).

          IF wl_imposto_operacao-taxtyp EQ c_icm3.
            IF me->zif_nf_writer~at_leis_estado-opertyp EQ 'T'.

              IF me->zif_nf_writer~at_categoria_nf-entrad EQ abap_true.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom = me->zif_nf_writer~at_shipfrom
                     AND shipto   = me->zif_nf_writer~at_shipto
                     AND gruop    = c_30
                     AND value    = me->zif_nf_writer~at_cabecalho-parid
                     AND value2   = c_item-matnr.

                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate base
                    FROM j_1btxic3
                    INTO wl_1btxic
                     WHERE land1    = c_br
                       AND shipfrom  = me->zif_nf_writer~at_shipfrom
                       AND shipto    = me->zif_nf_writer~at_shipto
                       AND gruop    = c_40
                       AND value    = me->zif_nf_writer~at_cabecalho-parid..

                  IF sy-subrc IS NOT INITIAL.
                    IF me->zif_nf_writer~at_parametro_fiscal-parvw NE c_br AND me->zif_nf_writer~at_parametro_fiscal-parvw NE c_ag.
                      SELECT SINGLE rate base
                        FROM j_1btxic2
                        INTO wl_1btxic
                         WHERE land1    = c_br
                           AND shipfrom  = me->zif_nf_writer~at_shipfrom
                           AND shipto    = me->zif_nf_writer~at_shipto
                           AND matnr    = c_item-matnr.
                    ENDIF.
                    IF sy-subrc IS NOT INITIAL.
                      SELECT SINGLE rate
                        FROM j_1btxic1
                        INTO wl_1btxic
                         WHERE land1    = c_br
                           AND shipfrom  = me->zif_nf_writer~at_shipfrom
                           AND shipto    = me->zif_nf_writer~at_shipto.

                    ENDIF.
                  ENDIF.
                ENDIF.

              ELSE.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom  = me->zif_nf_writer~at_shipfrom
                     AND shipto    = me->zif_nf_writer~at_shipto
                     AND gruop    = c_76
                     AND value    = me->zif_nf_writer~at_cabecalho-parid
                     AND value2    = c_item-matnr.

                IF sy-subrc IS NOT INITIAL.
                  IF me->zif_nf_writer~at_parametro_fiscal-parvw NE c_br
                  AND me->zif_nf_writer~at_parametro_fiscal-parvw NE c_ag.
                    SELECT SINGLE rate base
                      FROM j_1btxic2
                      INTO wl_1btxic
                       WHERE land1    = c_br
                         AND shipfrom  = me->zif_nf_writer~at_shipfrom
                         AND shipto    = me->zif_nf_writer~at_shipto
                         AND matnr    = c_item-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate
                      FROM j_1btxic1
                      INTO wl_1btxic
                       WHERE land1    = c_br
                         AND shipfrom = me->zif_nf_writer~at_shipfrom
                         AND shipto   = me->zif_nf_writer~at_shipto.
                  ENDIF.
                ENDIF.
              ENDIF.
              MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
              SELECT SINGLE *
                FROM t001w
                INTO wl_t001w
                 WHERE werks EQ me->zif_nf_writer~at_cabecalho-branch.
              IF sy-subrc IS INITIAL.
                IF ( me->zif_nf_writer~at_categoria_nf-direct NE '1' ).

                  SELECT SINGLE *
                    FROM a924
                    INTO wl_a924
                     WHERE kschl    EQ 'ZIVP'
                       AND aland    EQ 'BR'
                       AND txreg_sf EQ wl_t001w-regio
                       AND matnr    EQ c_item-matnr
                       AND datab    LE sy-datum
                       AND datbi    GE sy-datum.

                  IF sy-subrc IS INITIAL.


                    SELECT SINGLE *
                      FROM konp
                      INTO wl_konp
                       WHERE knumh EQ wl_a924-knumh.

                  ENDIF.
                ENDIF.
              ENDIF.

*-IR147555 - 20.09.2023 - LF - inicio
              READ TABLE me->zif_nf_writer~at_parametros_regulatorios
              ASSIGNING FIELD-SYMBOL(<fs_regula>) WITH TABLE KEY operacao = me->zif_nf_writer~at_cabecalho-operacao.
              IF sy-subrc = 0.
                IF me->zif_nf_writer~at_shipfrom = me->zif_nf_writer~at_shipto.
                  wl_1btxic-rate = <fs_regula>-rate.
                ENDIF.
              ENDIF.
*-IR147555 -  20.09.2023 - LF - fim



              IF wl_1btxic-base IS INITIAL.
                IF wl_konp-kbetr GT c_item-netpr.
                  c_item-netwr = c_item-menge * wl_konp-kbetr.
                ENDIF.
                wl_imposto-base   = c_item-netwr.
                wl_imposto-taxval = ( wl_imposto-base * ( wl_1btxic-rate / 100 ) ).
                wl_imposto-othbas = 0.

              ELSE.
                IF wl_konp-kbetr GT c_item-netpr.
                  c_item-netwr = c_item-menge * wl_konp-kbetr.
                ENDIF.
                wl_imposto-base   = c_item-netwr * ( wl_1btxic-base / 100 ).
                wl_imposto-taxval = wl_imposto-base * ( wl_1btxic-rate / 100 ).
                wl_imposto-othbas = c_item-netwr - wl_imposto-base.

              ENDIF.
              wl_imposto-rate = wl_1btxic-rate.
              IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
                CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                       wl_imposto-excbas.
              ENDIF.
              APPEND wl_imposto TO e_impostos.
              CLEAR: wl_imposto.
            ELSEIF me->zif_nf_writer~at_leis_estado-opertyp EQ c_i.
              "aqui outros tipos de operacoes
              MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
              MOVE: c_item-netwr TO wl_imposto-excbas.
              IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
                CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                       wl_imposto-excbas.
              ENDIF.
              APPEND wl_imposto TO e_impostos.
              CLEAR: wl_imposto.
            ELSEIF me->zif_nf_writer~at_leis_estado-opertyp EQ c_n.
              MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
              MOVE: c_item-netwr TO wl_imposto-othbas.
              IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
                CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                       wl_imposto-excbas.
              ENDIF.
              APPEND wl_imposto TO e_impostos.
              CLEAR: wl_imposto.
            ENDIF.
          ELSEIF wl_1btxsdc-pis EQ abap_true
             AND wl_imposto_operacao-taxtyp EQ c_ipis.

            SELECT SINGLE *
              FROM j_1btxpis
              INTO wl_1btxpis
               WHERE country EQ c_br
                 AND gruop   EQ c_72
                 AND value   EQ me->zif_nf_writer~at_cabecalho-branch.

            MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
            IF sy-subrc IS INITIAL.
              wl_imposto-base   = c_item-netwr. " +  v_ics1. "ALRS 28.03.2024
              wl_imposto-rate   = wl_1btxpis-rate.
              wl_imposto-taxval = wl_imposto-base * ( wl_1btxpis-rate / 100 ).
              wl_imposto-othbas = 0.
            ELSE.
              MOVE: c_item-netwr TO wl_imposto-othbas.
            ENDIF.
            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                     wl_imposto-excbas.
            ENDIF.
            APPEND wl_imposto TO e_impostos.
            CLEAR: wl_imposto, wl_1btxpis.

          ELSEIF wl_1btxsdc-cofins EQ abap_true
             AND wl_imposto_operacao-taxtyp EQ c_icof.
            SELECT SINGLE *
              FROM j_1btxcof
              INTO wl_1btxcof
               WHERE country EQ c_br
                 AND gruop   EQ c_71
                 AND value   EQ me->zif_nf_writer~at_cabecalho-branch.

            MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
            IF sy-subrc IS INITIAL.
              wl_imposto-base   = c_item-netwr. "  +  v_ics1. "ALRS 28.03.2024
              wl_imposto-rate   = wl_1btxcof-rate.
              IF  wl_imposto-base > 0 AND wl_1btxcof-rate  > 0.
                wl_imposto-taxval = wl_imposto-base * ( wl_1btxcof-rate / 100 ).
              ENDIF.
              wl_imposto-othbas = 0.
            ELSE.
              MOVE: c_item-netwr TO wl_imposto-othbas.
            ENDIF.

            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                     wl_imposto-excbas.
            ENDIF.
            APPEND wl_imposto TO e_impostos.
            CLEAR: wl_imposto, wl_1btxcof.

          ELSEIF  wl_imposto_operacao-taxtyp EQ c_ics1.

            IF me->zif_nf_writer~at_categoria_nf-entrad EQ abap_true.

              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom  = me->zif_nf_writer~at_shipfrom
                   AND shipto    = me->zif_nf_writer~at_shipto
                   AND gruop    = c_30
                   AND value    = me->zif_nf_writer~at_cabecalho-parid
                   AND value2    = c_item-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom  = me->zif_nf_writer~at_shipfrom
                     AND shipto    = me->zif_nf_writer~at_shipto
                     AND gruop    = c_40
                     AND value    = me->zif_nf_writer~at_cabecalho-parid.

                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate
                    FROM j_1btxic1
                    INTO wl_1btxic
                     WHERE land1    = c_br
                       AND shipfrom  = me->zif_nf_writer~at_shipfrom
                       AND shipto    = me->zif_nf_writer~at_shipto.

                ENDIF.

              ENDIF.

            ELSE.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom  = me->zif_nf_writer~at_shipfrom
                   AND shipto    = me->zif_nf_writer~at_shipto
                   AND gruop    = c_76
                   AND value    = me->zif_nf_writer~at_cabecalho-parid
                   AND value2    = c_item-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate
                  FROM j_1btxic1
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom = me->zif_nf_writer~at_shipfrom
                     AND shipto   = me->zif_nf_writer~at_shipto.
              ENDIF.

            ENDIF.
            MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.

            "MM-IR174020-Gera nota energia ZNFW JOB classe-ALRS
            wl_imposto-rate =  wl_1btxic-rate .
            IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
              wl_imposto-base = c_item-netwr / ( 1 - ( ( wl_1btxic-rate * ( wl_1btxic-base / 100 ) ) / 100 ) ).
            ENDIF.

            IF wl_imposto-base > 0 AND  wl_imposto-rate > 0.
              IF  wl_imposto-base > 0.
                wl_imposto-base   = wl_imposto-base * ( wl_1btxic-base / 100 ).
                wl_imposto-taxval = wl_imposto-base  * ( wl_imposto-rate / 100 ).
              ELSE.
                wl_imposto-taxval = wl_imposto-base * ( wl_imposto-rate / 100 ).
              ENDIF.
              v_ics1 = wl_imposto-taxval.
            ENDIF.

            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                     wl_imposto-excbas.
            ENDIF.
            "MM-IR174020-Gera nota energia ZNFW JOB classe-ALRS

*            wl_imposto-rate =  wl_1btxic-rate .
*            IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
*              wl_imposto-base = c_item-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
*            ENDIF.
*            IF wl_imposto-base > 0 AND  wl_imposto-rate > 0.
*              wl_imposto-taxval = wl_imposto-base * ( wl_imposto-rate / 100 ).
*            ENDIF.
*
*            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
*              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
*                     wl_imposto-excbas.
*            ENDIF.

            APPEND wl_imposto TO e_impostos.
            CLEAR: wl_imposto, wl_1btxic.
          ELSE.

            "Aqui outros impostos
            MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
            MOVE: c_item-netwr TO wl_imposto-othbas.

            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                     wl_imposto-excbas.
            ENDIF.

            APPEND wl_imposto TO e_impostos.
            CLEAR: wl_imposto.
          ENDIF.

        ENDLOOP.

      WHEN '2'. "Saídas

        LOOP AT me->zif_nf_writer~at_parametros_impostos INTO wl_imposto_operacao.

          IF wl_imposto_operacao-taxtyp EQ c_icm3.

            IF me->zif_nf_writer~at_leis_estado-opertyp EQ 'T'.

              IF me->zif_nf_writer~at_categoria_nf-entrad EQ abap_true.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom  = me->zif_nf_writer~at_shipfrom
                     AND shipto    = me->zif_nf_writer~at_shipto
                     AND gruop    = c_30
                     AND value    = me->zif_nf_writer~at_cabecalho-parid
                     AND value2    = c_item-matnr.

                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate base
                    FROM j_1btxic3
                    INTO wl_1btxic
                     WHERE land1    = c_br
                       AND shipfrom  = me->zif_nf_writer~at_shipfrom
                       AND shipto    = me->zif_nf_writer~at_shipto
                       AND gruop    = c_40
                       AND value    = me->zif_nf_writer~at_cabecalho-parid.

                  IF sy-subrc IS NOT INITIAL.
                    IF me->zif_nf_writer~at_parametro_fiscal-parvw NE c_br
                    AND me->zif_nf_writer~at_parametro_fiscal-parvw NE c_ag.
                      SELECT SINGLE rate base
                        FROM j_1btxic2
                        INTO wl_1btxic
                         WHERE land1    = c_br
                           AND shipfrom  = me->zif_nf_writer~at_shipfrom
                           AND shipto    = me->zif_nf_writer~at_shipto
                           AND matnr    = c_item-matnr.
                    ENDIF.
                    IF sy-subrc IS NOT INITIAL.
                      SELECT SINGLE rate
                        FROM j_1btxic1
                        INTO wl_1btxic
                         WHERE land1    = c_br
                           AND shipfrom  = me->zif_nf_writer~at_shipfrom
                           AND shipto    = me->zif_nf_writer~at_shipto.

                    ENDIF.
                  ENDIF.
                ENDIF.

              ELSE.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom  = me->zif_nf_writer~at_shipfrom
                     AND shipto    = me->zif_nf_writer~at_shipto
                     AND gruop    = c_76
                     AND value    = me->zif_nf_writer~at_cabecalho-parid
                     AND value2    = c_item-matnr.

                IF sy-subrc IS NOT INITIAL.
                  IF me->zif_nf_writer~at_parametro_fiscal-parvw NE c_br
                  AND me->zif_nf_writer~at_parametro_fiscal-parvw NE c_ag.
                    SELECT SINGLE rate base
                      FROM j_1btxic2
                      INTO wl_1btxic
                       WHERE land1    = c_br
                         AND shipfrom  = me->zif_nf_writer~at_shipfrom
                         AND shipto    = me->zif_nf_writer~at_shipto
                         AND matnr    = c_item-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate
                      FROM j_1btxic1
                      INTO wl_1btxic
                       WHERE land1    = c_br
                         AND shipfrom = me->zif_nf_writer~at_shipfrom
                         AND shipto   = me->zif_nf_writer~at_shipto.
                  ENDIF.
                ENDIF.
              ENDIF.
              MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
              SELECT SINGLE *
                FROM t001w
                INTO wl_t001w
                 WHERE werks EQ me->zif_nf_writer~at_cabecalho-branch.
              IF sy-subrc IS INITIAL.


                SELECT SINGLE *
                  FROM a924
                  INTO wl_a924
                   WHERE kschl    EQ 'ZIVP'
                     AND aland    EQ 'BR'
                     AND txreg_sf EQ wl_t001w-regio
                     AND matnr    EQ c_item-matnr
                     AND datab    LE sy-datum
                     AND datbi    GE sy-datum.

                IF sy-subrc IS INITIAL.


                  SELECT SINGLE *
                    FROM konp
                    INTO wl_konp
                     WHERE knumh EQ wl_a924-knumh.

                ENDIF.

              ENDIF.

*-IR147555 - 20.09.2023 - LF - inicio
              READ TABLE me->zif_nf_writer~at_parametros_regulatorios
              ASSIGNING <fs_regula> WITH TABLE KEY operacao = me->zif_nf_writer~at_cabecalho-operacao.
              IF sy-subrc = 0.
                IF me->zif_nf_writer~at_shipfrom = me->zif_nf_writer~at_shipto.
                  wl_1btxic-rate = <fs_regula>-rate.
                ENDIF.
              ENDIF.
*-IR147555 -  20.09.2023 - LF - fim

              IF wl_1btxic-base IS INITIAL.
                IF wl_konp-kbetr GT c_item-netpr.
                  c_item-netwr = c_item-menge * wl_konp-kbetr.
                ENDIF.
                wl_imposto-base   = c_item-netwr.
                wl_imposto-taxval = ( wl_imposto-base * ( wl_1btxic-rate / 100 ) ).
                wl_imposto-othbas = 0.

              ELSE.
                IF wl_konp-kbetr GT c_item-netpr.
                  c_item-netwr = c_item-menge * wl_konp-kbetr.
                ENDIF.
                wl_imposto-base   = c_item-netwr * ( wl_1btxic-base / 100 ).
                wl_imposto-taxval = wl_imposto-base * ( wl_1btxic-rate / 100 ).
                wl_imposto-othbas = c_item-netwr - wl_imposto-base.

              ENDIF.
              wl_imposto-rate = wl_1btxic-rate.
              IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
                CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                       wl_imposto-excbas.
              ENDIF.
              APPEND wl_imposto TO e_impostos.
              CLEAR: wl_imposto.
            ELSEIF me->zif_nf_writer~at_leis_estado-opertyp EQ c_i.
              "aqui outros tipos de operacoes
              MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
              MOVE: c_item-netwr TO wl_imposto-excbas.
              IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
                CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                       wl_imposto-excbas.
              ENDIF.
              APPEND wl_imposto TO e_impostos.
              CLEAR: wl_imposto.
            ELSEIF me->zif_nf_writer~at_leis_estado-opertyp EQ c_n.
              MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
              MOVE: c_item-netwr TO wl_imposto-othbas.
              IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
                CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                       wl_imposto-excbas.
              ENDIF.
              APPEND wl_imposto TO e_impostos.
              CLEAR: wl_imposto.
            ENDIF.
          ELSEIF wl_1btxsdc-pis EQ abap_true
             AND wl_imposto_operacao-taxtyp EQ c_ipis.

            SELECT SINGLE *
              FROM j_1btxpis
              INTO wl_1btxpis
               WHERE country EQ c_br
                 AND gruop   EQ c_72
                 AND value   EQ me->zif_nf_writer~at_cabecalho-branch.

            MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
            IF sy-subrc IS INITIAL.
              wl_imposto-base   = c_item-netwr. " + v_ics1.
              wl_imposto-rate   = wl_1btxpis-rate.
              wl_imposto-taxval = wl_imposto-base * ( wl_1btxpis-rate / 100 ).
              wl_imposto-othbas = 0.
            ELSE.
              MOVE: c_item-netwr TO wl_imposto-othbas.
            ENDIF.
            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                     wl_imposto-excbas.
            ENDIF.
            APPEND wl_imposto TO e_impostos.
            CLEAR: wl_imposto, wl_1btxpis.

          ELSEIF wl_1btxsdc-cofins EQ abap_true
             AND wl_imposto_operacao-taxtyp EQ c_icof.
            SELECT SINGLE *
              FROM j_1btxcof
              INTO wl_1btxcof
               WHERE country EQ c_br
                 AND gruop   EQ c_71
                 AND value   EQ me->zif_nf_writer~at_cabecalho-branch.

            MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
            IF sy-subrc IS INITIAL.
              wl_imposto-base   = c_item-netwr. " + v_ics1.
              wl_imposto-rate   = wl_1btxcof-rate.
              IF  wl_imposto-base > 0 AND wl_1btxcof-rate  > 0.
                wl_imposto-taxval = wl_imposto-base * ( wl_1btxcof-rate / 100 ).
              ENDIF.
              wl_imposto-othbas = 0.
            ELSE.
              MOVE: c_item-netwr TO wl_imposto-othbas.
            ENDIF.

            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                     wl_imposto-excbas.
            ENDIF.
            APPEND wl_imposto TO e_impostos.
            CLEAR: wl_imposto, wl_1btxcof.

          ELSEIF  wl_imposto_operacao-taxtyp EQ c_ics1.

            IF me->zif_nf_writer~at_categoria_nf-entrad EQ abap_true.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom  = me->zif_nf_writer~at_shipfrom
                   AND shipto    = me->zif_nf_writer~at_shipto
                   AND gruop    = c_30
                   AND value    = me->zif_nf_writer~at_cabecalho-parid
                   AND value2    = c_item-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom  = me->zif_nf_writer~at_shipfrom
                     AND shipto    = me->zif_nf_writer~at_shipto
                     AND gruop    = c_40
                     AND value    = me->zif_nf_writer~at_cabecalho-parid.

                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate
                    FROM j_1btxic1
                    INTO wl_1btxic
                     WHERE land1    = c_br
                       AND shipfrom  = me->zif_nf_writer~at_shipfrom
                       AND shipto    = me->zif_nf_writer~at_shipto.

                ENDIF.

              ENDIF.

            ELSE.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom  = me->zif_nf_writer~at_shipfrom
                   AND shipto    = me->zif_nf_writer~at_shipto
                   AND gruop    = c_76
                   AND value    = me->zif_nf_writer~at_cabecalho-parid
                   AND value2    = c_item-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate
                  FROM j_1btxic1
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom = me->zif_nf_writer~at_shipfrom
                     AND shipto   = me->zif_nf_writer~at_shipto.
              ENDIF.

            ENDIF.

            MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.

            "MM-IR174020-Gera nota energia ZNFW JOB classe-ALRS
            wl_imposto-rate =  wl_1btxic-rate .
            IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
              wl_imposto-base = c_item-netwr / ( 1 - ( ( wl_1btxic-rate * ( wl_1btxic-base / 100 ) ) / 100 ) ).
            ENDIF.

            IF wl_imposto-base > 0 AND  wl_imposto-rate > 0.
              IF  wl_imposto-base > 0.
                wl_imposto-base   = wl_imposto-base * ( wl_1btxic-base / 100 ).
                wl_imposto-taxval = wl_imposto-base  * ( wl_imposto-rate / 100 ).
              ELSE.
                wl_imposto-taxval = wl_imposto-base * ( wl_imposto-rate / 100 ).
              ENDIF.
              v_ics1 = wl_imposto-taxval.
            ENDIF.

            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                     wl_imposto-excbas.
            ENDIF.
            "MM-IR174020-Gera nota energia ZNFW JOB classe-ALRS

*            wl_imposto-rate =  wl_1btxic-rate .
*            IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
*              wl_imposto-base = c_item-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
*            ENDIF.
*            IF wl_imposto-base > 0 AND  wl_imposto-rate > 0.
*              wl_imposto-taxval = wl_imposto-base * ( wl_imposto-rate / 100 ).
*            ENDIF.
*
*
*            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
*              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
*                     wl_imposto-excbas.
*            ENDIF.

            APPEND wl_imposto TO e_impostos.
            CLEAR: wl_imposto, wl_1btxic.
          ELSE.

            "Aqui outros impostos
            MOVE-CORRESPONDING: wl_imposto_operacao TO wl_imposto.
            MOVE: c_item-netwr TO wl_imposto-othbas.

            IF me->zif_nf_writer~at_parametro_fiscal-complemento EQ 'S'.
              CLEAR: wl_imposto-rate, wl_imposto-base, wl_imposto-taxval, wl_imposto-othbas,
                     wl_imposto-excbas.
            ENDIF.

            APPEND wl_imposto TO e_impostos.
            CLEAR: wl_imposto.
          ENDIF.

        ENDLOOP.

    ENDCASE.

    LOOP AT e_impostos ASSIGNING FIELD-SYMBOL(<fs_imposto_item>).
      <fs_imposto_item>-itmnum = c_item-itmnum.
    ENDLOOP.

  ENDMETHOD.


  method ZIF_NF_WRITER~MONTA_MENSAGENS.

    R_IF_NF_WRITER = ME.

    DATA: WL_MENSAGEM TYPE ZFIWRT0013.

    LOOP AT ME->ZIF_NF_WRITER~AT_PARAMETROS_MENSAGENS INTO DATA(WL_MENSAGEM_PARAMETRO).

      WL_MENSAGEM-SEQNUM   = WL_MENSAGEM_PARAMETRO-SEQNUM.
      WL_MENSAGEM-LINNUM   = WL_MENSAGEM_PARAMETRO-LINNUM.
      WL_MENSAGEM-MESSAGE  = WL_MENSAGEM_PARAMETRO-MESSAGE.
      WL_MENSAGEM-manual =  'M'.
      APPEND WL_MENSAGEM TO ME->ZIF_NF_WRITER~AT_MENSAGENS.

    ENDLOOP.

  endmethod.


  METHOD zif_nf_writer~novo_lancamento.

    r_if_nf_writer = me.

    CLEAR:  "Dados Lançamento

            me->zif_nf_writer~at_cabecalho,
            me->zif_nf_writer~at_itens,
            me->zif_nf_writer~at_impostos,
            me->zif_nf_writer~at_contabil,
            me->zif_nf_writer~at_mov_estoque,
            me->zif_nf_writer~at_mensagens,
            me->zif_nf_writer~at_parceiros,
            me->zif_nf_writer~at_fatura,
            me->zif_nf_writer~at_docs_referenciados,
            me->zif_nf_writer~at_categoria_nf,
            me->zif_nf_writer~at_shipfrom,
            me->zif_nf_writer~at_shipto,
            me->zif_nf_writer~at_leis_estado,
            me->zif_nf_writer~at_rateio,


            "Parametros
            me->zif_nf_writer~at_parametro_fiscal,
            me->zif_nf_writer~at_parametros_impostos,
            me->zif_nf_writer~at_parametros_contabil,
            me->zif_nf_writer~at_parametros_mov_estoque,
            me->zif_nf_writer~at_parametros_mensagens,
            me->zif_nf_writer~at_parametros_leis_estado,

*-IR147555 - 20.09.2023 - LF - inicio
            me->zif_nf_writer~at_parametros_regulatorios.
*-IR147555 - 20.09.2023 - LF - inicio

  ENDMETHOD.


  METHOD zif_nf_writer~prepara_lancamento.

    r_if_nf_writer = me.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Impostos
*----------------------------------------------------------------------------------------------------------------------------------*

    SELECT *
      FROM zfiwrt0002 INTO TABLE me->zif_nf_writer~at_parametros_impostos
     WHERE operacao EQ me->zif_nf_writer~at_parametro_fiscal-operacao.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Contabil
*----------------------------------------------------------------------------------------------------------------------------------*

    SELECT *
      FROM zfiwrt0003 INTO TABLE me->zif_nf_writer~at_parametros_contabil
     WHERE operacao EQ me->zif_nf_writer~at_parametro_fiscal-operacao.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Mov. Estoque
*----------------------------------------------------------------------------------------------------------------------------------*

    SELECT *
      FROM zfiwrt0004 INTO TABLE me->zif_nf_writer~at_parametros_mov_estoque
     WHERE operacao EQ me->zif_nf_writer~at_parametro_fiscal-operacao.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Mensagens
*----------------------------------------------------------------------------------------------------------------------------------*

    SELECT *
      FROM zfiwrt0005 INTO TABLE me->zif_nf_writer~at_parametros_mensagens
     WHERE operacao EQ me->zif_nf_writer~at_parametro_fiscal-operacao.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Leis Dentro/Fora Estado
*----------------------------------------------------------------------------------------------------------------------------------*

    SELECT *
      FROM zfiwrt0006 INTO TABLE me->zif_nf_writer~at_parametros_leis_estado
     WHERE operacao EQ me->zif_nf_writer~at_parametro_fiscal-operacao.


*----------------------------------------------------------------------------------------------------------------------------------*
*   Determinar Operação Dentro Fora Estado
*----------------------------------------------------------------------------------------------------------------------------------*

    me->zif_nf_writer~determina_fora_dentro_estado( IMPORTING e_indcoper     =  DATA(_indcoper)
                                                              e_texto_fiscal =  DATA(_texto_fiscal) ).

    READ TABLE me->zif_nf_writer~at_parametros_leis_estado INTO me->zif_nf_writer~at_leis_estado WITH KEY indcoper = _indcoper.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Configurar Cabeçalho
*----------------------------------------------------------------------------------------------------------------------------------*

    me->zif_nf_writer~at_cabecalho-parvw            = me->zif_nf_writer~at_parametro_fiscal-parvw.
    me->zif_nf_writer~at_cabecalho-nftype           = me->zif_nf_writer~at_parametro_fiscal-nftype.
    me->zif_nf_writer~at_cabecalho-ctrl_zrfl        = me->zif_nf_writer~at_parametro_fiscal-ctrl_zrfl.
    me->zif_nf_writer~at_cabecalho-zpesagem         = me->zif_nf_writer~at_parametro_fiscal-zpesagem.
    me->zif_nf_writer~at_cabecalho-dias             = me->zif_nf_writer~at_parametro_fiscal-dias.
    me->zif_nf_writer~at_cabecalho-retorno          = me->zif_nf_writer~at_parametro_fiscal-retorno.
    me->zif_nf_writer~at_cabecalho-energia          = me->zif_nf_writer~at_parametro_fiscal-energia.
    me->zif_nf_writer~at_cabecalho-servico          = me->zif_nf_writer~at_parametro_fiscal-servico.
    me->zif_nf_writer~at_cabecalho-complemento      = me->zif_nf_writer~at_parametro_fiscal-complemento.
    me->zif_nf_writer~at_cabecalho-referencia       = me->zif_nf_writer~at_parametro_fiscal-referencia.

    me->zif_nf_writer~at_cabecalho-cfop             = me->zif_nf_writer~at_leis_estado-cfop.
    me->zif_nf_writer~at_cabecalho-taxlw1           = me->zif_nf_writer~at_leis_estado-taxlw1.
    me->zif_nf_writer~at_cabecalho-taxlw2           = me->zif_nf_writer~at_leis_estado-taxlw2.
    me->zif_nf_writer~at_cabecalho-taxlw4           = me->zif_nf_writer~at_leis_estado-taxlw4.
    me->zif_nf_writer~at_cabecalho-taxlw5           = me->zif_nf_writer~at_leis_estado-taxlw5.
    me->zif_nf_writer~at_cabecalho-opertyp          = me->zif_nf_writer~at_leis_estado-opertyp.
    me->zif_nf_writer~at_cabecalho-taxcode          = me->zif_nf_writer~at_leis_estado-taxcode.


    "Administração
    me->zif_nf_writer~at_cabecalho-usnam            = sy-uname.
    me->zif_nf_writer~at_cabecalho-dt_criacao       = sy-datum.
    me->zif_nf_writer~at_cabecalho-hr_criacao       = sy-uzeit.
    me->zif_nf_writer~at_cabecalho-usuario_ult_mod  = sy-uname.
    me->zif_nf_writer~at_cabecalho-dt_ult_mod       = sy-datum.
    me->zif_nf_writer~at_cabecalho-hr_ult_mod       = sy-uzeit.


*----------------------------------------------------------------------------------------------------------------------------------*
*   Configurar Itens
*----------------------------------------------------------------------------------------------------------------------------------*

    LOOP AT me->zif_nf_writer~at_itens ASSIGNING FIELD-SYMBOL(<fs_item>).

      <fs_item>-cfop    = me->zif_nf_writer~at_leis_estado-cfop.
      <fs_item>-itmtyp  = me->zif_nf_writer~at_parametro_fiscal-itmtyp.

    ENDLOOP.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Montagem Impostos
*----------------------------------------------------------------------------------------------------------------------------------*

    LOOP AT me->zif_nf_writer~at_itens INTO DATA(wa_item).

*-CS2020001225 - 20.09.2021 - JT - inicio
      me->zif_nf_writer~monta_impostos(  EXPORTING i_impostos = i_impostos[]
                                         IMPORTING e_impostos = DATA(t_impostos_item)
                                          CHANGING c_item     = wa_item ).
*-CS2020001225 - 20.09.2021 - JT - fim
      LOOP AT t_impostos_item INTO DATA(wl_imposto_item).
        APPEND wl_imposto_item TO me->zif_nf_writer~at_impostos.
      ENDLOOP.

    ENDLOOP.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Montagem Contabil
*----------------------------------------------------------------------------------------------------------------------------------*

*-CS2020001225 - 20.09.2021 - JT - inicio
    me->zif_nf_writer~monta_contabil( EXPORTING i_impostos = i_impostos[]
                                                i_contabil = i_contabil[]
                                      IMPORTING e_contabil = DATA(t_contabil) ).
*-CS2020001225 - 20.09.2021 - JT - fim

    LOOP AT t_contabil INTO DATA(wl_contabil).
      APPEND wl_contabil TO me->zif_nf_writer~at_contabil.
    ENDLOOP.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Montagem Estoque
*----------------------------------------------------------------------------------------------------------------------------------*

    DATA: wl_mov_estoque TYPE zfiwrt0012.

    LOOP AT me->zif_nf_writer~at_parametros_mov_estoque INTO DATA(wl_mov_estoque_parametro).
      CLEAR: wl_mov_estoque.

      MOVE-CORRESPONDING wl_mov_estoque_parametro TO wl_mov_estoque.

      APPEND wl_mov_estoque TO me->zif_nf_writer~at_mov_estoque.
    ENDLOOP.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Montagem Mensagens
*----------------------------------------------------------------------------------------------------------------------------------*

    me->zif_nf_writer~monta_mensagens( ).

  ENDMETHOD.


  method ZIF_NF_WRITER~SET_CABECALHO.

    R_IF_NF_WRITER = ME.

    ME->ZIF_NF_WRITER~AT_CABECALHO = I_CABECALHO.

  endmethod.


  method ZIF_NF_WRITER~SET_DADOS_TRANSP.

    R_IF_NF_WRITER = ME.

    ME->ZIF_NF_WRITER~AT_DADOS_TRANSP = I_DADOS_TRANSP.

  endmethod.


  METHOD zif_nf_writer~set_fatura_energia.

    r_if_nf_writer = me.

    me->zif_nf_writer~at_fatura = i_fatura.

  ENDMETHOD.


  METHOD zif_nf_writer~set_monta_contabil.

    r_if_nf_writer = me.

    LOOP AT i_contabil INTO DATA(w_contabil).
      APPEND w_contabil  TO me->zif_nf_writer~at_contabil.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_nf_writer~set_monta_fiscal.

    r_if_nf_writer = me.

    FREE: me->zif_nf_writer~at_impostos.

    LOOP AT i_fiscal INTO DATA(w_fiscal).
      APPEND w_fiscal  TO me->zif_nf_writer~at_impostos.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_nf_writer~set_monta_mensagens.

    r_if_nf_writer = me.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Mensagens
*----------------------------------------------------------------------------------------------------------------------------------*
    IF i_mensagens[] IS INITIAL.
      SELECT *
        FROM zfiwrt0005 INTO TABLE me->zif_nf_writer~at_parametros_mensagens
       WHERE operacao EQ me->zif_nf_writer~at_cabecalho-operacao.
    ELSE.
      me->zif_nf_writer~at_parametros_mensagens[] = i_mensagens[].
    ENDIF.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Montagem Mensagens
*----------------------------------------------------------------------------------------------------------------------------------*
    me->zif_nf_writer~monta_mensagens( ).

  ENDMETHOD.


  METHOD zif_nf_writer~set_monta_mov_estoque.

    DATA: wl_mov_estoque TYPE zfiwrt0012.

    r_if_nf_writer = me.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Mov. Estoque
*----------------------------------------------------------------------------------------------------------------------------------*
    IF i_mov_estoque[] IS INITIAL.
      SELECT *
        FROM zfiwrt0004
        INTO TABLE me->zif_nf_writer~at_parametros_mov_estoque
       WHERE operacao EQ me->zif_nf_writer~at_cabecalho-operacao.
    ELSE.
      me->zif_nf_writer~at_parametros_mov_estoque[] = i_mov_estoque[].
    ENDIF.

    LOOP AT me->zif_nf_writer~at_parametros_mov_estoque INTO DATA(wl_mov_estoque_parametro).
      CLEAR: wl_mov_estoque.
      MOVE-CORRESPONDING wl_mov_estoque_parametro         TO wl_mov_estoque.
      APPEND wl_mov_estoque                               TO me->zif_nf_writer~at_mov_estoque.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_nf_writer~set_parametro_contabil.

    r_if_nf_writer = me.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Contabil
*----------------------------------------------------------------------------------------------------------------------------------*
    IF i_parametro_contabil[] IS INITIAL.
      SELECT *
        FROM zfiwrt0003
        INTO TABLE me->zif_nf_writer~at_parametros_contabil
       WHERE operacao = me->zif_nf_writer~at_cabecalho-operacao.
    ELSE.
      me->zif_nf_writer~at_parametros_contabil[] = i_parametro_contabil[].
    ENDIF.

  ENDMETHOD.


  METHOD zif_nf_writer~set_parametro_fiscal.

    r_if_nf_writer = me.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Parâmetros Fiscais NF.WRITER Contabil
*----------------------------------------------------------------------------------------------------------------------------------*
    IF i_parametro_fiscal[] IS INITIAL.
      SELECT *
        FROM zfiwrt0002
        INTO TABLE me->zif_nf_writer~at_parametros_impostos
       WHERE operacao EQ me->zif_nf_writer~at_cabecalho-operacao.
    ELSE.
      me->zif_nf_writer~at_parametros_impostos[] = i_parametro_fiscal[].
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_NF_WRITER~SET_RATEIO.

    R_IF_NF_WRITER = ME.

    APPEND I_RATEIO TO ME->ZIF_NF_WRITER~AT_RATEIO.

  ENDMETHOD.


  METHOD zif_nf_writer~validar_registro.

    r_if_nf_writer = me.

*-------------------------------------------------------------------------------------------------*
*  Validar Cabeçalho
*-------------------------------------------------------------------------------------------------*

    SELECT SINGLE *
      FROM zfiwrt0001 INTO me->zif_nf_writer~at_parametro_fiscal
     WHERE operacao EQ me->zif_nf_writer~at_cabecalho-operacao.

    IF ( sy-subrc NE 0 ) OR ( me->zif_nf_writer~at_cabecalho-operacao IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_nf_writer
        EXPORTING
          textid = VALUE #( msgid = zcx_nf_writer=>zcx_operacao_not_found-msgid
                            msgno = zcx_nf_writer=>zcx_operacao_not_found-msgno
                            attr1 = CONV #( me->zif_nf_writer~at_cabecalho-operacao )
                            )
          msgty  = 'E'
          msgno  = zcx_nf_writer=>zcx_operacao_not_found-msgno
          msgid  = zcx_nf_writer=>zcx_operacao_not_found-msgid
          msgv1  = CONV #( me->zif_nf_writer~at_cabecalho-operacao ).
    ENDIF.

    IF me->zif_nf_writer~at_parametro_fiscal-lm_estoque NE 'S'.

      SELECT SINGLE *
        FROM j_1baa INTO me->zif_nf_writer~at_categoria_nf
       WHERE nftype EQ me->zif_nf_writer~at_parametro_fiscal-nftype.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_nf_writer
          EXPORTING
            textid = VALUE #( msgid = zcx_nf_writer=>zcx_nf_quebra_gerada_retorno-msgid
                              msgno = zcx_nf_writer=>zcx_nf_quebra_gerada_retorno-msgno
                              attr1 = CONV #( me->zif_nf_writer~at_parametro_fiscal-nftype )
                              )
            msgty  = 'E'
            msgno  = zcx_nf_writer=>zcx_nf_quebra_gerada_retorno-msgno
            msgid  = zcx_nf_writer=>zcx_nf_quebra_gerada_retorno-msgid
            msgv1  = CONV #( me->zif_nf_writer~at_parametro_fiscal-nftype ).
      ENDIF.

    ENDIF.

    IF me->zif_nf_writer~at_cabecalho-docnum_retorno IS NOT INITIAL.

      SELECT SINGLE *
        FROM zfiwrt0008 INTO @DATA(wl_zfiwrt008)
       WHERE docnum_retorno   EQ @me->zif_nf_writer~at_cabecalho-docnum_retorno
         AND docs_estornados EQ @abap_false
         AND loekz           EQ @abap_false
         AND OPERACAO        EQ @me->zif_nf_writer~at_cabecalho-operacao.

      IF sy-subrc EQ 0.
        RAISE EXCEPTION TYPE zcx_nf_writer
          EXPORTING
            textid = VALUE #( msgid = zcx_nf_writer=>zcx_nf_quebra_gerada_retorno-msgid
                              msgno = zcx_nf_writer=>zcx_nf_quebra_gerada_retorno-msgno
                              attr1 = CONV #( wl_zfiwrt008-seq_lcto )
                              attr2 = CONV #( me->zif_nf_writer~at_cabecalho-docnum_retorno )
                              )
            msgty  = 'E'
            msgno  = zcx_nf_writer=>zcx_nf_quebra_gerada_retorno-msgno
            msgid  = zcx_nf_writer=>zcx_nf_quebra_gerada_retorno-msgid
            msgv1  = CONV #( wl_zfiwrt008-seq_lcto )
            msgv2  = CONV #( me->zif_nf_writer~at_cabecalho-docnum_retorno ).
      ENDIF.


      SELECT SINGLE *
        FROM setleaf INTO @DATA(_wl_setleaf_zsdt0163)
       WHERE setname = 'ZSDT0163_CONFIG'
         AND valfrom = 'CK_RET_AUTH'.

      IF sy-subrc NE 0.
        SELECT SINGLE *
          FROM j_1bnfe_active INTO @DATA(_active_ret)
         WHERE docnum = @me->zif_nf_writer~at_cabecalho-docnum_retorno
           AND docsta = '1'
           AND cancel = ' '.

        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_nf_writer
            EXPORTING
              textid = VALUE #( msgid = zcx_nf_writer=>zcx_doc_retorno_nao_auth-msgid
                                msgno = zcx_nf_writer=>zcx_doc_retorno_nao_auth-msgno
                                attr1 = CONV #( me->zif_nf_writer~at_cabecalho-docnum_retorno )
                                )
              msgty  = 'E'
              msgno  = zcx_nf_writer=>zcx_doc_retorno_nao_auth-msgno
              msgid  = zcx_nf_writer=>zcx_doc_retorno_nao_auth-msgid
              msgv1  = CONV #( me->zif_nf_writer~at_cabecalho-docnum_retorno ).
        ENDIF.
      ENDIF.

    ENDIF.

*-------------------------------------------------------------------------------------------------*
*  Validar Itens
*-------------------------------------------------------------------------------------------------*

    IF me->zif_nf_writer~at_itens[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_nf_writer
        EXPORTING
          textid = VALUE #( msgid = zcx_nf_writer=>zcx_itens_not_found-msgid
                            msgno = zcx_nf_writer=>zcx_itens_not_found-msgno )
          msgty  = 'E'
          msgno  = zcx_nf_writer=>zcx_itens_not_found-msgno
          msgid  = zcx_nf_writer=>zcx_itens_not_found-msgid.
    ENDIF.


  ENDMETHOD.


  method ZIF_NF_WRITER~VALIDA_CONTABILIZACAO.



  endmethod.


  METHOD zif_nf_writer~set_parametro_regulatorio.

    DATA: t_set    TYPE STANDARD TABLE OF rgsb4,
          w_set    TYPE rgsb4,
          t_regula TYPE zif_nf_writer=>ty_regula_t,
          w_regula TYPE zif_nf_writer=>ty_regula.

    r_if_nf_writer = me.

*----------------------------------------------------------------------------------------------------------------------------------*
*   Tabela de Sets Regulatorios - ICMS (transação GS02)
*----------------------------------------------------------------------------------------------------------------------------------*
    IF i_regula[] IS INITIAL.

      "Get the icms Set values
      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          class           = '0000'
          setnr           = 'SET_MAGGI_ZNFW_ICMS'
          no_descriptions = abap_false
        TABLES
          set_values      = t_set
        EXCEPTIONS
          set_not_found   = 1
          OTHERS          = 2.

      LOOP AT t_set    INTO w_set.
        CLEAR w_regula.
        w_regula-operacao = w_set-from.
        w_regula-rate     = w_set-title.

        "Only unique lines
        READ TABLE t_regula TRANSPORTING NO FIELDS
        WITH TABLE KEY operacao = w_regula-operacao.
        IF sy-subrc IS NOT INITIAL.
          INSERT w_regula  INTO TABLE t_regula.
        ENDIF.
      ENDLOOP.

      me->zif_nf_writer~at_parametros_regulatorios[] = t_regula[].

    ELSE.
      me->zif_nf_writer~at_parametros_regulatorios[] = i_regula[].
    ENDIF.
  ENDMETHOD.
ENDCLASS.
