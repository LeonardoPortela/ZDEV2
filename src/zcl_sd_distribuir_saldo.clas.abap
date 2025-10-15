class ZCL_SD_DISTRIBUIR_SALDO definition
  public
  final
  create public .

public section.

  data GT_MSG type BAPIRET2_TAB .
  data AT_NRO_SOL type ZDE_NRO_SOL .
  data AT_SEQ type NUMC3 .
  data AT_VBELN type VBELN_VA .
  data AT_POSNR type POSNR_VA .
  data AT_SPART type SPART .

  methods BUSCA_DADOS
    importing
      !CG_DADOS_CABEC type ZSDS093_TT
    changing
      !CG_DADOS type ZSDS094_TT .
  methods BUSCA_DADOS_COPY
    changing
      !CG_DADOS type ZSDS094_TT .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_zmm0132,
             matnr TYPE mard-matnr,   "Material
             werks TYPE mard-werks,   "Centro
             lgort TYPE mard-lgort,   "Depósito
             clabs TYPE mchb-clabs,   "Saldo
             charg TYPE mchb-charg,   "Lote
             licha TYPE mcha-licha,   "Lote Fornecedor
             vfdat TYPE mch1-vfdat,   "Data Validade
             lifnr TYPE mslb-lifnr,   "fornecedor armz
             lblab TYPE mslb-lblab,   "Saldo Fornecedor
             name1 TYPE lfa1-name1,   "Nome do fornecedor
           END OF ty_zmm0132.

    TYPES: BEGIN OF ty_mat,
             matnr TYPE mara-matnr,
             maktx TYPE makt-maktx,
             meins TYPE mara-meins,
             mtart TYPE mara-mtart,
           END OF ty_mat.

    TYPES: BEGIN OF ty_ped,
             ebeln       TYPE ekko-ebeln,
             ebelp       TYPE ekpo-ebelp,
             lifnr       TYPE ekko-lifnr,
             bukrs       TYPE ekko-bukrs,
             matnr       TYPE ekpo-matnr,
             werks       TYPE ekpo-werks,
             lgort       TYPE ekpo-lgort,
             menge       TYPE ekpo-menge,
             menge_fat   TYPE ekpo-menge,
             menge_saldo TYPE ekpo-menge,
           END OF ty_ped.

    DATA gt_dados TYPE zsds094_tt .
    DATA gt_mat TYPE TABLE OF ty_mat .
    DATA gt_pedido TYPE TABLE OF ty_ped .
    DATA gt_zmm0132 TYPE TABLE OF ty_zmm0132.
    DATA gv_dummy TYPE string .
    DATA:
      gr_material TYPE RANGE OF matnr .
    DATA:
     gr_centro TYPE RANGE OF werks_d .

    METHODS set_init
      IMPORTING
        !it_dados TYPE zsds094_tt .
    METHODS busca_materiais .
    METHODS msg .
    METHODS busca_zmm0132 .
    METHODS check_error
      RETURNING
        VALUE(rv_ok) TYPE boolean .
    METHODS busca_zsdt0051.
    METHODS busca_centros.
    METHODS preenche_dados.
    METHODS busca_marca.
ENDCLASS.



CLASS ZCL_SD_DISTRIBUIR_SALDO IMPLEMENTATION.


  METHOD busca_dados.

    TYPES: BEGIN OF ty_obj,
             matnr TYPE char18, "matnr,
             charg TYPE charg_d,
           END OF ty_obj.

    DATA: lc_distribuicao_insumos TYPE REF TO zcl_distribuicao_insumos,  "*-CS2025000249-16.06.2025-#182039-JT
          t_zsdt0411              TYPE zsdt0411_t,                       "*-CS2025000249-16.06.2025-#182039-JT
          t_zsdt0415              TYPE zsdt0415_t,                       "*-CS2025000249-16.06.2025-#182039-JT
          w_zsdt0082              TYPE zsdt0082,                         "*-CS2025000249-16.06.2025-#182039-JT
          w_dados                 TYPE zsds094,                          "*-CS2025000249-16.06.2025-#182039-JT
          lv_pend_aprov           TYPE char01,                           "*-CS2025000249-16.06.2025-#182039-JT
          lv_utilizado            TYPE dzmeng,                           "*-CS2025000249-16.06.2025-#182039-JT
          lv_object               TYPE ausp-objek,                       "*-CS2025000249-16.06.2025-#182039-JT
          ls_obj                  TYPE ty_obj,                           "*-CS2025000249-16.06.2025-#182039-JT
          lv_categ_semente        TYPE atwrt,                            "*-CS2025000249-16.06.2025-#182039-JT
          lv_space                TYPE char22 VALUE '                      ',
          lc_mm_util              TYPE REF TO zcl_mm_util.               "*-CS2025000249-16.06.2025-#182039-JT

    CREATE OBJECT: lc_distribuicao_insumos, lc_mm_util.  "*-CS2025000249-16.06.2025-#182039-JT

    set_init( cg_dados ).

*-CS2025000249-16.06.2025-#182039-JT-inicio
    FREE: t_zsdt0415.

    lv_pend_aprov  = abap_false.

    READ TABLE cg_dados_cabec INTO DATA(_dados_cabec) INDEX 1.

    READ TABLE cg_dados       INTO DATA(_dados) INDEX 1.
    me->at_nro_sol = _dados-nro_sol.
    me->at_seq     = _dados-seq.
    me->at_vbeln   = _dados-vbeln.
    me->at_posnr   = _dados-posnr.
    me->at_spart   = _dados_cabec-spart.  "*-US191954-30.09.2025-#191954-JT

    w_zsdt0082     = lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = _dados-nro_sol i_seq   = _dados-seq
                                                            i_vbeln   = _dados-vbeln   i_posnr = _dados-posnr ).
*   lc_distribuicao_insumos->get_zsdt0411( EXPORTING i_nro_sol = _dados-nro_sol i_todos = abap_true IMPORTING e_zsdt0411 = t_zsdt0411 ).
*
*   LOOP AT t_zsdt0411 INTO DATA(_zsdt0411) WHERE nro_sol = _dados-nro_sol
*                                             AND status <> 'A'. "aprovado
*     EXIT.
*   ENDLOOP.
*   IF sy-subrc = 0.
*     lv_pend_aprov = abap_true.
*   ENDIF.

    t_zsdt0415 = lc_distribuicao_insumos->get_zsdt0415( i_ch_referencia = w_zsdt0082-ch_referencia ).

    DELETE t_zsdt0415 WHERE status = 'C'.
    SORT t_zsdt0415 BY ch_referencia item_distrib.
    DELETE ADJACENT DUPLICATES FROM t_zsdt0415 COMPARING ch_referencia item_distrib.

    lc_distribuicao_insumos->set_table_zsdt0415( t_zsdt0415 ).
*
*   FREE: cg_dados.
*   LOOP AT t_zsdt0415           INTO DATA(_zsdt0415).
*     MOVE-CORRESPONDING _zsdt0415 TO w_dados.
*     APPEND w_dados               TO cg_dados.
*   ENDLOOP.
*-CS2025000249-16.06.2025-#182039-JT-fim

    busca_materiais(  ).

    busca_centros(  ).

    busca_zmm0132(  ).

    busca_zsdt0051( ).

    preenche_dados( ).

    busca_marca( ).

    cg_dados = gt_dados.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    LOOP AT cg_dados  ASSIGNING FIELD-SYMBOL(<fs_dados>).

*---------------------------------------------------------------------------------------------
*     Determinação Ponto de Coleta
*---------------------------------------------------------------------------------------------
      IF <fs_dados>-ebeln IS NOT INITIAL.  "Embarque Fornecedor

        <fs_dados>-lifnr_pc = <fs_dados>-lifnr_pedido.

      ELSEIF <fs_dados>-lifnr IS NOT INITIAL. "Embarque CD Terceiro

        <fs_dados>-lifnr_pc = <fs_dados>-lifnr.

      ELSEIF <fs_dados>-werks IS NOT INITIAL. "Embarque Filial

        <fs_dados>-lifnr_pc = <fs_dados>-werks.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_dados>-lifnr_pc
          IMPORTING
            output = <fs_dados>-lifnr_pc.

      ENDIF.

    ENDLOOP.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


*-CS2025000249-16.06.2025-#182039-JT-inicio
    DATA(_definido_estoque_utilizar) = abap_false.
    LOOP AT cg_dados            INTO _dados.
      DATA(lv_tabix)               = sy-tabix.

      _dados-flexibilidade         = 1.
      _dados-carga_auto            = 'X'.

      MOVE _dados                 TO w_dados.

      IF w_zsdt0082-bloqueio = abap_true.
        DATA(_0415) = lc_distribuicao_insumos->get_table_zsdt0415( _dados ).
        IF _0415 IS INITIAL.
          DELETE cg_dados INDEX lv_tabix.
          CONTINUE.
        ENDIF.
        MOVE               _dados   TO w_dados.
        MOVE-CORRESPONDING _0415    TO _dados.
        w_dados-kwmeng               = _0415-qte_sol.
        _dados-clabs                 = w_dados-clabs.
      ENDIF.

      FREE: _dados-menge_consumo, _dados-kwmeng.

      DATA(t_dados_0415)           =  lc_distribuicao_insumos->get_dados_zsdt0415( i_zsds093 = _dados_cabec i_zsds094 = _dados ).

      "DATA(_del_registro) = abap_false.
      LOOP AT t_dados_0415      INTO DATA(_dados_0415).

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP  29-08-2025 --->>>
*        IF ( _dados_0415-mesma_solic     = abap_true  AND w_zsdt0082-bloqueio = abap_true ) OR
*           ( _dados_0415-solic_pai       = abap_true ) OR
*           ( _dados_0415-solic_cancelada = abap_true ).
*          CONTINUE.
*        ENDIF.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - 29-08-2025  WPP <<----

        IF _dados_0415-nro_sol      = w_zsdt0082-nro_sol_origem AND
           _dados_0415-matnr        = _dados_cabec-matnr        AND
           _dados_0415-lgort        = _dados_cabec-lgort        AND
           "_dados_0415-lifnr_pc     = _dados_cabec-lifnr_pc     AND  Não é necessário checar ponto de coleta
           _dados_0415-werks        = _dados_cabec-werks        AND
           "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
           w_zsdt0082-charg         = _dados-charg             AND
           w_zsdt0082-lifnr_arm     = _dados-lifnr             AND
           w_zsdt0082-ebeln         = _dados-ebeln             AND
           w_zsdt0082-ebelp         = _dados-ebelp.
          "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

          _dados-dt_entrega             = _dados_0415-dt_entrega.
          _dados-nr_rot_pc              = _dados_0415-nr_rot_pc.
          _dados-prioridade             = _dados_0415-prioridade.
          _dados-flexibilidade          = _dados_0415-flexibilidade.
          _dados-carga_auto             = _dados_0415-carga_auto.
          _dados-transf_no_fornecedor   = _dados_0415-transf_no_fornecedor.
          _dados-kwmeng                 = _dados_0415-qte_sol.
          _dados-nao_editar             = abap_true.
          _definido_estoque_utilizar    = abap_true. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

        ELSE.

          IF w_zsdt0082-bloqueio = abap_true.
            _dados-kwmeng          = w_dados-kwmeng.
          ENDIF.

          IF ( _dados_0415-mesma_solic     = abap_true  AND w_zsdt0082-bloqueio = abap_true ) OR
             ( _dados_0415-solic_pai       = abap_true ) OR
             ( _dados_0415-solic_cancelada = abap_true ).
            CONTINUE.
          ENDIF.

          _dados_0415-qte_sol      =  lc_distribuicao_insumos->set_analisar_estoque( _dados_0415 ). "Determina se deve ser contabilizado o consumo da liberação
          _dados-menge_consumo     = _dados-menge_consumo + _dados_0415-qte_sol.
          _dados-clabs             = _dados-clabs         - _dados_0415-qte_sol.

        ENDIF.
      ENDLOOP.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      IF _dados-ebeln IS NOT INITIAL.
        IF _dados-menge_fat >= _dados-menge_ped AND _dados-menge_consumo IS INITIAL. "Se pedido foi todo faturado, remover linha...
          DELETE cg_dados INDEX lv_tabix.
          CONTINUE.
        ENDIF.

        "Verificar consumo do Pedido por solicitação de recebimento(Transação ZMM0235)
        DATA(_consumo) = lc_distribuicao_insumos->check_uso_estoque_sol_ent( EXPORTING i_estoque_pedido = _dados ).
        ADD _consumo TO _dados-menge_consumo.
        SUBTRACT _consumo FROM _dados-clabs.

      ELSE.
        "*-US192801-06.10.2025-#192801-JT
        IF _dados-menge_consumo IS INITIAL AND _dados-clabs IS INITIAL AND _dados-nao_editar = abap_false AND w_zsdt0082-bloqueio = abap_false.
          DELETE cg_dados INDEX lv_tabix.
          CONTINUE.
        ENDIF.

      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

      CLEAR: ls_obj, _dados-categ_semente, _dados-germinacao.
*     ls_obj-matnr = _dados-matnr(18).
*     ls_obj-charg = _dados-charg.
*     lv_object    = ls_obj.

      CONCATENATE _dados-matnr _dados-charg INTO lv_object SEPARATED BY lv_space.

      CALL METHOD lc_mm_util->get_caracteristica_geral
        EXPORTING
          i_object               = lv_object
          i_caracteristica       = 'SEMENTE_CATEGORIA'
        IMPORTING
          e_valor_caracteristica = lv_categ_semente.

      IF lv_categ_semente = '?'.
        lv_categ_semente = 0.
      ENDIF.

      SELECT SINGLE nome
        INTO _dados-categ_semente
        FROM zsdt0199
       WHERE id_cat_sementes = lv_categ_semente.

      CALL METHOD lc_mm_util->get_caracteristica_geral
        EXPORTING
          i_object               = lv_object
          i_caracteristica       = 'SEMENTE_GERMINACAO'
        IMPORTING
          e_valor_caracteristica = _dados-germinacao.

      IF _dados-germinacao = '?'.
        _dados-germinacao = abap_off.
      ENDIF.

      SELECT SINGLE ort01
        INTO @DATA(_ort01)
        FROM lfa1
       WHERE lifnr = @_dados-lifnr.

      IF sy-subrc = 0.
        _dados-municipio_forn = _ort01.
      ENDIF.

      SELECT SINGLE name1
        INTO @DATA(_name1)
        FROM lfa1
       WHERE lifnr = @_dados-lifnr_pedido.

      IF sy-subrc = 0.
        _dados-razao_social_forn_ped = _name1.
      ENDIF.

      MODIFY cg_dados           FROM _dados INDEX lv_tabix.
    ENDLOOP.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    "*-US192801-06.10.2025-#192801-JT
    IF _definido_estoque_utilizar EQ abap_true AND w_zsdt0082-bloqueio = abap_false.
      DELETE cg_dados WHERE NOT ( nao_editar EQ abap_true AND kwmeng > 0 ).
    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


*   DELETE cg_dados WHERE clabs = 0.
    LOOP AT cg_dados  INTO _dados.
      DATA(l_tabix) = sy-tabix.

      "*-US192801-06.10.2025-#192801-JT
      IF _dados-clabs = 0 AND _dados-menge_fat = _dados-menge_consumo AND _dados-nao_editar = abap_false AND w_zsdt0082-bloqueio = abap_false.
        DELETE cg_dados INDEX l_tabix.
        CONTINUE.
      ENDIF.

      IF _dados-ebeln IS NOT INITIAL AND _dados-ebelp IS NOT INITIAL.
        SELECT SINGLE netpr
          INTO @DATA(_netpr)
          FROM ekpo
         WHERE ebeln = @_dados-ebeln
           AND ebelp = @_dados-ebelp.

        IF sy-subrc = 0 AND _dados-clabs = 1 AND _dados-menge_ped = 1 AND _netpr = '0.01'.
          DELETE cg_dados INDEX l_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT cg_dados BY werks lifnr lgort ebeln ebelp matnr.
*-CS2025000249-16.06.2025-#182039-JT-fim

  ENDMETHOD.


  METHOD set_init.
    CLEAR: gt_dados.
    gt_dados = it_dados.

  ENDMETHOD.


  METHOD busca_materiais.

    DATA: lt_dados TYPE zsds094_tt.
    DATA: lr_mat TYPE RANGE OF cuobn.

    LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).
      APPEND INITIAL LINE TO lr_mat ASSIGNING FIELD-SYMBOL(<fs_mat>).
      <fs_mat>-sign = 'I'.
      <fs_mat>-option = 'EQ'.
      <fs_mat>-low = <fs_dados>-matnr.
    ENDLOOP.

*-US191954-30.09.2025-#191954-JT-inicio
    READ TABLE gt_dados INTO DATA(_dados) INDEX 1.
    CHECK sy-subrc = 0.

    IF     _dados-spart = '04'. "sementes
      SELECT material, valorclasf
          FROM zi_mm_mat_caract
          WHERE material IN @lr_mat
             AND classificacao = 'NOVA_CULTIVAR'
          INTO TABLE @DATA(lt_materiais).
    ELSEIF _dados-spart = '03'. "defensivos
      SELECT material, valorclasf
          FROM zi_mm_mat_caract
          WHERE material IN @lr_mat
             AND classificacao = 'PRINCICIO_ATIVO'
          INTO TABLE @lt_materiais.
    ELSE.
      RETURN.
    ENDIF.
*-US191954-30.09.2025-#191954-JT-fim

    IF sy-subrc = 0.
      SELECT material
       FROM zi_mm_mat_caract
       FOR ALL ENTRIES IN @lt_materiais
       WHERE valorclasf = @lt_materiais-valorclasf
       INTO TABLE @DATA(lt_materiais2).

      IF sy-subrc = 0.
        gr_material = VALUE #( FOR ls_mat IN lt_materiais2
                              ( sign = 'I'
                                option = 'EQ'
                                low = ls_mat  ) ).

        SELECT a~matnr, b~maktx, a~meins, a~mtart
            FROM mara AS a
            INNER JOIN makt AS b
            ON a~matnr = b~matnr
            WHERE a~matnr IN @gr_material
            INTO TABLE @gt_mat.

      ELSE.
        "não foi possivel localizar os materiais nova_cultivar
        MESSAGE e001 INTO gv_dummy.
        msg( ).
      ENDIF.

    ELSE.

      gr_material = VALUE #( FOR ls_dados IN gt_dados
                            ( sign = 'I'
                              option = 'EQ'
                              low = ls_dados-matnr  ) ).

      SELECT a~matnr, b~maktx, a~meins, a~mtart
          FROM mara AS a
          INNER JOIN makt AS b
          ON a~matnr = b~matnr
          WHERE a~matnr IN @gr_material
          INTO TABLE @gt_mat.

    ENDIF.

  ENDMETHOD.


  METHOD msg.

    APPEND VALUE bapiret2(
        id = sy-msgid
        number = sy-msgno
        type = sy-msgty
        message_v1 = sy-msgv1
        message_v2 = sy-msgv2
        message_v3 = sy-msgv3
        message_v4 = sy-msgv4
     ) TO gt_msg.

  ENDMETHOD.


  METHOD busca_zmm0132.

    DATA: ls_zmm0132 TYPE ty_zmm0132.

    DATA: lv_lblab TYPE mslb-lblab,
          lv_lgort TYPE mard-lgort.  "*-CS2025000249-16.06.2025-#182039-JT

    CHECK check_error( ).
    CHECK gt_dados IS NOT INITIAL.

    SELECT matnr, werks, lgort, charg, clabs, cinsm, cspem
      FROM mchb
      INTO TABLE @DATA(lt_mchb)
*      FOR ALL ENTRIES IN @gt_dados
      WHERE matnr IN @gr_material
      AND   werks IN @gr_centro. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
*      AND   lgort = @gt_dados-lgort
    " AND   ( clabs NE 0 OR cinsm NE 0  OR cspem NE 0 ).
    IF sy-subrc = 0.

      SELECT matnr, charg, vfdat, licha
       FROM mch1
       INTO TABLE @DATA(lt_mch1)
       FOR ALL ENTRIES IN @lt_mchb
       WHERE matnr EQ @lt_mchb-matnr
       AND   charg EQ @lt_mchb-charg.

      SELECT charg, vfdat, lfabr
       FROM zppt0011
       INTO TABLE @DATA(lt_zppt0011)
       FOR ALL ENTRIES IN @lt_mchb
       WHERE charg = @lt_mchb-charg.

      SELECT matnr, werks, lgort, chargd, zlicha
        FROM zppt0016
        INTO TABLE @DATA(lt_zppt0016)
        FOR ALL ENTRIES IN @lt_mchb
        WHERE matnr  = @lt_mchb-matnr
          AND werks  = @lt_mchb-werks
          AND lgort  = @lt_mchb-lgort
          AND chargd = @lt_mchb-charg.

      SELECT matnr, werks, charg, hsdat, vfdat
          FROM mcha
         INTO TABLE @DATA(lt_mcha)
         FOR ALL ENTRIES IN @lt_mchb
        WHERE matnr EQ @lt_mchb-matnr
          AND werks EQ @lt_mchb-werks
          AND charg EQ @lt_mchb-charg.

      SELECT matnr, werks
         FROM marc
         INTO TABLE @DATA(lt_marc)
         FOR ALL ENTRIES IN @lt_mchb
        WHERE matnr EQ @lt_mchb-matnr
          AND werks EQ @lt_mchb-werks.

      IF lt_marc IS NOT INITIAL.
        SORT lt_marc BY matnr werks.
        DELETE ADJACENT DUPLICATES FROM lt_marc COMPARING matnr werks.
      ENDIF.

    ENDIF.

    "estoque especial
    SELECT matnr, werks, charg, lifnr, lblab, lbins
        FROM mslb AS a
        INTO TABLE @DATA(lt_mslb)
*          FOR ALL ENTRIES IN @gt_dados
        WHERE matnr IN @gr_material
          AND werks IN @gr_centro.
    "AND ( lblab NE 0 OR lbins NE 0 ). "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

    IF sy-subrc = 0.
*-CS2025000249-16.06.2025-#182039-JT-inicio
*      SELECT matnr, werks, lgort, labst
*        FROM mard
*        INTO TABLE @DATA(lt_mard2)
*         FOR ALL ENTRIES IN @lt_mslb
*       WHERE matnr  = @lt_mslb-matnr
*         AND werks  = @lt_mslb-werks.
*-CS2025000249-16.06.2025-#182039-JT-fim

      SELECT matnr, charg, vfdat, licha
        FROM mch1
        APPENDING TABLE @lt_mch1
        FOR ALL ENTRIES IN @lt_mslb
        WHERE matnr EQ @lt_mslb-matnr
        AND   charg EQ @lt_mslb-charg.

      SELECT lifnr, name1
        FROM lfa1
        INTO TABLE @DATA(lt_lfa1)
        FOR ALL ENTRIES IN @lt_mslb
        WHERE lifnr EQ @lt_mslb-lifnr.

    ENDIF.

    SELECT matnr, werks, lgort, labst
      FROM mard
      INTO TABLE @DATA(lt_mard)
*      FOR ALL ENTRIES IN @gt_dados
      WHERE matnr IN @gr_material
      AND   werks IN @gr_centro
*      AND   lgort = @gt_dados-lgort
      AND   labst NE 0.

    IF sy-subrc = 0.

      SELECT matnr, werks
            FROM marc
          APPENDING TABLE @lt_marc
          FOR ALL ENTRIES IN @lt_mard
         WHERE matnr EQ @lt_mard-matnr
           AND werks EQ @lt_mard-werks.

      IF lt_marc IS NOT INITIAL.
        SORT lt_marc BY matnr werks.
        DELETE ADJACENT DUPLICATES FROM lt_marc COMPARING matnr werks.
      ENDIF.

    ENDIF.

    FREE: gt_zmm0132..

    SORT: lt_marc BY matnr werks,
          lt_mard BY matnr werks.

    LOOP AT lt_mard ASSIGNING FIELD-SYMBOL(<fs_mard>).

      READ TABLE lt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb>)
             WITH KEY matnr = <fs_mard>-matnr.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CLEAR ls_zmm0132.
      ls_zmm0132-matnr = <fs_mard>-matnr.
      ls_zmm0132-werks = <fs_mard>-werks.
      ls_zmm0132-lgort = <fs_mard>-lgort.
      ls_zmm0132-clabs = <fs_mard>-labst.
      APPEND ls_zmm0132 TO gt_zmm0132.

    ENDLOOP.


    SORT: lt_marc BY matnr werks,
          lt_mchb BY matnr werks charg,
          lt_mslb BY matnr werks charg.

    LOOP AT lt_mchb ASSIGNING <fs_mchb>.

      CLEAR ls_zmm0132.
      ls_zmm0132-matnr = <fs_mchb>-matnr.
      ls_zmm0132-werks = <fs_mchb>-werks.
      ls_zmm0132-lgort = <fs_mchb>-lgort.
      ls_zmm0132-charg = <fs_mchb>-charg.

      READ TABLE lt_mch1 INTO DATA(ls_mch1)
                   WITH KEY matnr = <fs_mchb>-matnr
                            charg = <fs_mchb>-charg.

      READ TABLE lt_zppt0011 INTO DATA(ls_zppt0011)
                             WITH KEY charg = <fs_mchb>-charg.


      IF ls_zppt0011-vfdat IS NOT INITIAL AND ls_mch1-vfdat IS INITIAL.
        ls_zmm0132-vfdat = ls_zppt0011-vfdat.
      ELSEIF ls_zppt0011-vfdat IS INITIAL AND ls_mch1-vfdat IS NOT INITIAL.
        ls_zmm0132-vfdat = ls_mch1-vfdat.
      ELSEIF ls_zppt0011-vfdat IS NOT INITIAL AND ls_mch1-vfdat IS NOT INITIAL.
        ls_zmm0132-vfdat = ls_zppt0011-vfdat.
      ELSEIF ls_zppt0011-vfdat IS INITIAL AND ls_mch1-vfdat IS INITIAL.
        ls_zmm0132-vfdat = ls_mch1-vfdat.
      ENDIF.


      DATA(lv_zppt0011) = VALUE #( lt_zppt0011[ charg = <fs_mchb>-charg ]-lfabr OPTIONAL ).

      DATA(lv_zppt0016) = VALUE #( lt_zppt0016[ chargd = <fs_mchb>-charg ]-zlicha OPTIONAL ).

      ls_zmm0132-licha = COND #( WHEN lv_zppt0011   IS NOT INITIAL THEN lv_zppt0011
                               WHEN ls_mch1-licha IS NOT INITIAL THEN ls_mch1-licha
                               WHEN lv_zppt0016   IS NOT INITIAL THEN lv_zppt0016
                               ELSE '' ).

      ls_zmm0132-clabs = <fs_mchb>-clabs + <fs_mchb>-cinsm + <fs_mchb>-cspem.


      DATA(_append_line_mchb) = abap_false.
      LOOP AT lt_mslb ASSIGNING FIELD-SYMBOL(<fs_mslb>) WHERE matnr = <fs_mchb>-matnr
                                                          AND werks = <fs_mchb>-werks
                                                          AND charg = <fs_mchb>-charg.

        CLEAR: lv_lblab, lv_lgort.
        lv_lblab = <fs_mslb>-lblab + <fs_mslb>-lbins.

        ls_zmm0132-lifnr = <fs_mslb>-lifnr.

        READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
                        WITH KEY lifnr = <fs_mslb>-lifnr.

        IF ls_zmm0132-lifnr IS NOT INITIAL. "AND lv_lblab IS NOT INITIAL. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
          DATA(vg_lifnr) = ls_zmm0132-lifnr.
          DATA(vb_qtde)  = lv_lblab.
          ls_zmm0132-lifnr = ' '.
          APPEND ls_zmm0132 TO gt_zmm0132.
          _append_line_mchb = abap_true.
        ENDIF.

        "IF lv_lblab IS NOT INITIAL. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
        ls_zmm0132-lgort = 'SEME'.
        ls_zmm0132-clabs = ' '.
        ls_zmm0132-lifnr = vg_lifnr.
        ls_zmm0132-name1 = <fs_lfa1>-name1.
        ls_zmm0132-lblab = lv_lblab.
        APPEND ls_zmm0132 TO gt_zmm0132.
        "ENDIF. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

      ENDLOOP.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
*      IF ls_zmm0132-clabs IS NOT INITIAL.
      IF _append_line_mchb EQ abap_false.
        APPEND ls_zmm0132 TO gt_zmm0132.
      ENDIF.
*      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    ENDLOOP.

    SORT: lt_mchb BY matnr werks charg,
          lt_mslb BY matnr werks charg.


    LOOP AT lt_mslb ASSIGNING <fs_mslb>.

      CLEAR lv_lgort. "*-CS2025000249-16.06.2025-#182039-JT
      CLEAR ls_zmm0132.

      READ TABLE lt_mchb ASSIGNING <fs_mchb>
                         WITH KEY matnr = <fs_mslb>-matnr
                                  werks = <fs_mslb>-werks
                                  charg = <fs_mslb>-charg.

      IF sy-subrc = 0. ""Se ja foi processado
        CONTINUE.
      ENDIF.

      READ TABLE lt_mch1 INTO ls_mch1
                         WITH KEY matnr = <fs_mslb>-matnr
                                  charg = <fs_mslb>-charg.

      READ TABLE lt_zppt0011 INTO ls_zppt0011
                             WITH KEY charg = <fs_mslb>-charg.

      ls_zmm0132-matnr = <fs_mslb>-matnr.
      ls_zmm0132-werks = <fs_mslb>-werks.
      ls_zmm0132-lgort = 'SEME'.
      ls_zmm0132-charg = <fs_mslb>-charg.

      IF ls_zppt0011-vfdat IS NOT INITIAL AND ls_mch1-vfdat IS INITIAL.
        ls_zmm0132-vfdat = ls_zppt0011-vfdat.
      ELSEIF ls_zppt0011-vfdat IS INITIAL AND ls_mch1-vfdat IS NOT INITIAL.
        ls_zmm0132-vfdat = ls_mch1-vfdat.
      ELSEIF ls_zppt0011-vfdat IS NOT INITIAL AND ls_mch1-vfdat IS NOT INITIAL.
        ls_zmm0132-vfdat = ls_zppt0011-vfdat.
      ELSEIF ls_zppt0011-vfdat IS INITIAL AND ls_mch1-vfdat IS INITIAL.
        ls_zmm0132-vfdat = ls_mch1-vfdat.
      ENDIF.

      ls_zmm0132-licha = ls_mch1-licha.

      ls_zmm0132-clabs = 0.
      ls_zmm0132-lifnr = <fs_mslb>-lifnr.

      CLEAR: lv_lblab.
      lv_lblab = <fs_mslb>-lblab + <fs_mslb>-lbins.

*      READ TABLE lt_mcha ASSIGNING FIELD-SYMBOL(<fs_mcha>)
*                         WITH KEY matnr = <fs_mchb>-matnr
*                                  werks = <fs_mchb>-werks
*                                  charg = <fs_mchb>-charg.
*
*      IF sy-subrc EQ 0.
**        ls_zmm0132-hsdat =  ws_mcha-hsdat.
**        ls_zmm0132-vfdat =  ws_mcha-vfdat.
**        ls_zmm0132-charg =  ws_mcha-charg.
*      ENDIF.

      "Comentando, porque esta usando a mesma logica do loop anterior, quando está dentro do loop da mchb...
*      IF ls_zmm0132-lifnr IS NOT INITIAL AND lv_lblab IS NOT INITIAL.
*        vg_lifnr = ls_zmm0132-lifnr.
*        ls_zmm0132-lifnr = ' '.
*        ls_zmm0132-lblab = ' '.
*        APPEND ls_zmm0132 TO gt_zmm0132.
*      ENDIF.


      READ TABLE lt_lfa1 ASSIGNING <fs_lfa1>
                         WITH KEY lifnr = <fs_mslb>-lifnr.

      "IF lv_lblab IS NOT INITIAL.  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
      ls_zmm0132-lgort = 'SEME'.
      ls_zmm0132-clabs = ' '.
      ls_zmm0132-lifnr = ls_zmm0132-lifnr.
      ls_zmm0132-name1 = <fs_lfa1>-name1.
      ls_zmm0132-lblab = lv_lblab.
      APPEND ls_zmm0132 TO gt_zmm0132.
      "ENDIF.  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

    ENDLOOP.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
*    DELETE gt_zmm0132 WHERE clabs IS INITIAL AND
*                            lblab IS INITIAL.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


  ENDMETHOD.


  METHOD check_error.

    READ TABLE gt_msg TRANSPORTING NO FIELDS
            WITH KEY type = 'E'.
    IF sy-subrc = 0.
      rv_ok = abap_false.
    ELSE.
      rv_ok = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD busca_zsdt0051.

    DATA: lv_dt_ini TYPE datum,
          lv_dt_fim TYPE datum.
    DATA: lr_bsart TYPE RANGE OF ekko-bsart.
    DATA: ls_pedido TYPE ty_ped.

    lr_bsart = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZSEM ')
                        ( sign = 'I' option = 'EQ' low = 'ZEFI ')
                        ( sign = 'I' option = 'EQ' low = 'ZSON ') ).

    lv_dt_ini = lv_dt_fim = sy-datum.
    lv_dt_ini(4) = lv_dt_ini(4) - 1.

    SELECT a~ebeln,
           b~ebelp,
           a~bsart,
           a~lifnr,
           a~bukrs,
           b~matnr,
           b~werks,
           b~lgort,
           b~menge,
           b~meins
     FROM ekko AS a
     INNER JOIN ekpo AS b
     ON a~ebeln = b~ebeln
*     FOR ALL ENTRIES IN @gt_dados
    WHERE a~bsart   IN @lr_bsart
*      AND a~bukrs = gt_dados-
*      AND a~lifnr   = @gt_dados-lifnr
      AND a~bedat BETWEEN @lv_dt_ini AND @lv_dt_fim
      AND b~matnr IN @gr_material
      AND b~loekz IS INITIAL
      AND b~werks IN @gr_centro   "*-US189941-15.09.2025-#189941-JT-inicio

      INTO TABLE @DATA(lt_pedido).

    IF sy-subrc = 0.

      SELECT ebeln, ebelp, zekkn, vgabe, gjahr, belnr, buzei, budat, menge, dmbtr, shkzg
     FROM ekbe
     INTO TABLE @DATA(lt_ekbe)
     FOR ALL ENTRIES IN @lt_pedido
       WHERE ebeln = @lt_pedido-ebeln
         AND ebelp = @lt_pedido-ebelp
         AND vgabe = '2'.

    ENDIF.

    SORT: lt_ekbe BY ebeln ebelp.

    LOOP AT lt_pedido ASSIGNING FIELD-SYMBOL(<fs_ped>).

      CLEAR: ls_pedido.
      ls_pedido-ebeln = <fs_ped>-ebeln.
      ls_pedido-ebelp = <fs_ped>-ebelp.
      ls_pedido-lifnr = <fs_ped>-lifnr.
      ls_pedido-bukrs = <fs_ped>-bukrs.
      ls_pedido-matnr = <fs_ped>-matnr.
      ls_pedido-werks = <fs_ped>-werks.
      ls_pedido-lgort = <fs_ped>-lgort.
      ls_pedido-menge = <fs_ped>-menge.

      READ TABLE lt_ekbe TRANSPORTING NO FIELDS
                  WITH KEY ebeln = <fs_ped>-ebeln
                           ebelp = <fs_ped>-ebelp
                           BINARY SEARCH.
      IF sy-subrc = 0.

        LOOP AT lt_ekbe ASSIGNING FIELD-SYMBOL(<fs_ekbe>) FROM sy-tabix.

          IF <fs_ekbe>-ebeln <> <fs_ped>-ebeln
             OR <fs_ekbe>-ebelp <> <fs_ped>-ebelp.
            EXIT.
          ENDIF.

          IF <fs_ekbe>-shkzg = 'H'.
            ls_pedido-menge_fat = ls_pedido-menge_fat - <fs_ekbe>-menge.
          ELSE.
            ls_pedido-menge_fat = ls_pedido-menge_fat + <fs_ekbe>-menge .
          ENDIF.
*         COLLECT ls_pedido INTO gt_pedido.
        ENDLOOP.
        COLLECT ls_pedido INTO gt_pedido.
      ELSE.
        COLLECT ls_pedido INTO gt_pedido.
      ENDIF.

    ENDLOOP.

    LOOP AT gt_pedido ASSIGNING FIELD-SYMBOL(<fs_saldo>).
      <fs_saldo>-menge_saldo = <fs_saldo>-menge - <fs_saldo>-menge_fat.
    ENDLOOP.


  ENDMETHOD.


  METHOD busca_centros.

    CLEAR: gr_centro.

    READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>) INDEX 1.
    IF sy-subrc = 0.

      SELECT SINGLE vkorg
          FROM t001w
          INTO @DATA(lv_vkorg)
          WHERE werks = @<fs_dados>-werks.
      IF sy-subrc = 0.

        SELECT werks
            FROM t001w
          INTO TABLE @DATA(lt_werks)
          WHERE vkorg = @lv_vkorg.

        gr_centro = VALUE #( FOR ls_werks IN lt_werks
                               ( sign = 'I'
                                 option = 'EQ'
                                 low = ls_werks-werks  )  ).

      ENDIF.

    ENDIF.

    IF gr_centro IS INITIAL.
      "Não foi possivel localizar centros para o material
      MESSAGE e002 INTO gv_dummy.
      msg( ).

    ENDIF.

  ENDMETHOD.


  METHOD preenche_dados.

    DATA: ls_dados TYPE zsds094.

    SORT: gt_zmm0132 BY matnr,
          gt_mat BY matnr.

    CLEAR: gt_dados.

    LOOP AT gr_material ASSIGNING FIELD-SYMBOL(<fs_mat>).

      READ TABLE gt_mat ASSIGNING FIELD-SYMBOL(<fs_material>)
                          WITH KEY matnr = <fs_mat>-low
                          BINARY SEARCH.
      CHECK sy-subrc = 0.

      LOOP AT gt_zmm0132 ASSIGNING FIELD-SYMBOL(<fs_zmm0132>)
                          WHERE matnr = <fs_material>-matnr.

        CLEAR: ls_dados.

        ls_dados-nro_sol = me->at_nro_sol.
        ls_dados-seq     = me->at_seq.
        ls_dados-vbeln   = me->at_vbeln.
        ls_dados-posnr   = me->at_posnr.
        ls_dados-spart   = me->at_spart.  "*-US191954-30.09.2025-#191954-JT

        ls_dados-matnr = <fs_material>-matnr.
        ls_dados-arktx = <fs_material>-maktx.
        ls_dados-meins = <fs_material>-meins.
        ls_dados-mtart = <fs_material>-mtart.

        ls_dados-werks = <fs_zmm0132>-werks.
        ls_dados-lgort = <fs_zmm0132>-lgort.
        ls_dados-charg = <fs_zmm0132>-charg.
        ls_dados-licha = <fs_zmm0132>-licha.
        ls_dados-lifnr = <fs_zmm0132>-lifnr.
        IF <fs_zmm0132>-clabs IS NOT INITIAL.
          ls_dados-clabs = <fs_zmm0132>-clabs.
        ELSE.
          ls_dados-clabs = <fs_zmm0132>-lblab.
        ENDIF.
        ls_dados-vfdat = <fs_zmm0132>-vfdat.

        COLLECT ls_dados INTO gt_dados.

      ENDLOOP.

      LOOP AT gt_pedido ASSIGNING FIELD-SYMBOL(<fs_ped>)
                          WHERE matnr = <fs_material>-matnr.

        CLEAR: ls_dados.


        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        SELECT SINGLE lifnr INTO ls_dados-lifnr_pedido
          FROM ekko
         WHERE ebeln = <fs_ped>-ebeln.

        IF NOT ( sy-subrc = 0 AND ls_dados-lifnr_pedido IS NOT INITIAL ).
          SELECT SINGLE lifn2 INTO ls_dados-lifnr_pedido
            FROM ekpa
           WHERE ebeln = <fs_ped>-ebeln
             AND parvw = 'LF'.
        ENDIF.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


        ls_dados-nro_sol = me->at_nro_sol.
        ls_dados-seq     = me->at_seq.
        ls_dados-vbeln   = me->at_vbeln.
        ls_dados-posnr   = me->at_posnr.
        ls_dados-spart   = me->at_spart.  "*-US191954-30.09.2025-#191954-JT

        ls_dados-matnr = <fs_material>-matnr.
        ls_dados-arktx = <fs_material>-maktx.
        ls_dados-meins = <fs_material>-meins.
        ls_dados-mtart = <fs_material>-mtart.

        ls_dados-ebeln = <fs_ped>-ebeln.
        ls_dados-ebelp = <fs_ped>-ebelp.
        ls_dados-werks = <fs_ped>-werks.
        ls_dados-lgort = <fs_ped>-lgort.
        ls_dados-charg = abap_off.  "'99999999999'.
        ls_dados-menge_ped = <fs_ped>-menge.
        ls_dados-menge_fat = <fs_ped>-menge_fat.
        ls_dados-menge_saldo = <fs_ped>-menge_saldo.
        ls_dados-clabs = <fs_ped>-menge_saldo.

        COLLECT ls_dados INTO gt_dados.

      ENDLOOP.

    ENDLOOP.

*   DELETE gt_dados WHERE clabs IS INITIAL.

  ENDMETHOD.


  METHOD busca_marca.

    DATA: lv_object TYPE ausp-objek,
          lv_class  TYPE klah-class,
          lv_matnr  TYPE dfbatch-matnr,
          lv_charg  TYPE dfbatch-charg,
          lv_space  TYPE char22 VALUE '                      '.
    DATA: lt_class      TYPE TABLE OF sclass,
          lt_objectdata TYPE TABLE OF clobjdat.

    LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<fs_dado>).

      CHECK <fs_dado>-matnr IS NOT INITIAL
        AND <fs_dado>-charg IS NOT INITIAL.

      lv_matnr = <fs_dado>-matnr.
      lv_charg = <fs_dado>-charg.

* Material + Lote
      CONCATENATE lv_matnr
                  lv_charg
             INTO lv_object SEPARATED BY lv_space.

      CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
        EXPORTING
          classtype          = '023'
          object             = lv_object
          objecttable        = 'MCH1'
        TABLES
          t_class            = lt_class
          t_objectdata       = lt_objectdata
        EXCEPTIONS
          no_classification  = 1
          no_classtypes      = 2
          invalid_class_type = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      DATA(lv_marca) = VALUE #( lt_objectdata[ smbez = 'MARCA SEMENTE' ]-ausp1 OPTIONAL ).
      IF lv_marca <> '?'.
        <fs_dado>-marca = lv_marca.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD busca_dados_copy.

    DATA: lc_distribuicao_insumos TYPE REF TO zcl_distribuicao_insumos,  "*-CS2025000249-16.06.2025-#182039-JT
          t_zsdt0411              TYPE zsdt0411_t,                       "*-CS2025000249-16.06.2025-#182039-JT
          t_zsdt0415              TYPE zsdt0415_t,                       "*-CS2025000249-16.06.2025-#182039-JT
          w_zsdt0082              TYPE zsdt0082,                         "*-CS2025000249-16.06.2025-#182039-JT
          w_dados                 TYPE zsds094,                          "*-CS2025000249-16.06.2025-#182039-JT
          lv_pend_aprov           TYPE char01,                           "*-CS2025000249-16.06.2025-#182039-JT
          lv_utilizado            TYPE dzmeng.                           "*-CS2025000249-16.06.2025-#182039-JT

    CREATE OBJECT lc_distribuicao_insumos.  "*-CS2025000249-16.06.2025-#182039-JT

    set_init( cg_dados ).

*-CS2025000249-16.06.2025-#182039-JT-inicio
    FREE: t_zsdt0415.

    lv_pend_aprov  = abap_false.

    READ TABLE cg_dados INTO DATA(_dados) INDEX 1.
    me->at_nro_sol = _dados-nro_sol.
    me->at_seq     = _dados-seq.
    me->at_vbeln   = _dados-vbeln.
    me->at_posnr   = _dados-posnr.

    w_zsdt0082     = lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = _dados-nro_sol i_seq   = _dados-seq
                                                            i_vbeln   = _dados-vbeln   i_posnr = _dados-posnr ).
*   lc_distribuicao_insumos->get_zsdt0411( EXPORTING i_nro_sol = _dados-nro_sol i_todos = abap_true IMPORTING e_zsdt0411 = t_zsdt0411 ).
*
*   LOOP AT t_zsdt0411 INTO DATA(_zsdt0411) WHERE nro_sol = _dados-nro_sol
*                                             AND status <> 'A'. "aprovado
*     EXIT.
*   ENDLOOP.
*   IF sy-subrc = 0.
*     lv_pend_aprov = abap_true.
*   ENDIF.

    t_zsdt0415 = lc_distribuicao_insumos->get_zsdt0415( i_ch_referencia = w_zsdt0082-ch_referencia ).

    DELETE t_zsdt0415 WHERE status = 'C'.
    SORT t_zsdt0415 BY ch_referencia item_distrib.
    DELETE ADJACENT DUPLICATES FROM t_zsdt0415 COMPARING ch_referencia item_distrib.

    lc_distribuicao_insumos->set_table_zsdt0415( t_zsdt0415 ).
*
*   FREE: cg_dados.
*   LOOP AT t_zsdt0415           INTO DATA(_zsdt0415).
*     MOVE-CORRESPONDING _zsdt0415 TO w_dados.
*     APPEND w_dados               TO cg_dados.
*   ENDLOOP.
*-CS2025000249-16.06.2025-#182039-JT-fim

    busca_materiais(  ).

    busca_centros(  ).

    busca_zmm0132(  ).

    busca_zsdt0051( ).

    preenche_dados( ).

    busca_marca( ).

    cg_dados = gt_dados.

*-CS2025000249-16.06.2025-#182039-JT-inicio
    LOOP AT cg_dados            INTO _dados.
      DATA(lv_tabix)               = sy-tabix.

      _dados-flexibilidade         = 1.
      _dados-carga_auto            = 'X'.

      IF w_zsdt0082-bloqueio = abap_true.
        DATA(_0415) = lc_distribuicao_insumos->get_table_zsdt0415( _dados ).
        IF _0415 IS INITIAL.
          DELETE cg_dados INDEX lv_tabix.
          CONTINUE.
        ENDIF.
        MOVE               _dados TO w_dados.
        MOVE-CORRESPONDING _0415  TO _dados.
        _dados-kwmeng              = _0415-qte_sol.
        _dados-clabs               = w_dados-clabs.
      ENDIF.

      DATA: _dados_cabec TYPE zsds093.
      DATA(t_dados_0415)           =  lc_distribuicao_insumos->get_dados_zsdt0415( i_zsds093 = _dados_cabec i_zsds094 = _dados ).

      LOOP AT t_dados_0415      INTO DATA(_dados_0415).
        SELECT SINGLE werks
          INTO @DATA(_werks)
          FROM vbap
         WHERE vbeln = @w_zsdt0082-vbeln.

        IF _dados_0415-nro_sol = w_zsdt0082-nro_sol_origem AND
           _dados_0415-werks   = _werks                    AND
           w_zsdt0082-bloqueio = abap_false.
          FREE: _dados-menge_consumo.
        ENDIF.
      ENDLOOP.

      LOOP AT t_dados_0415      INTO _dados_0415.
        SELECT SINGLE werks
          INTO @_werks
          FROM vbap
         WHERE vbeln = @w_zsdt0082-vbeln.

        IF _dados_0415-nro_sol = w_zsdt0082-nro_sol_origem AND
           _dados_0415-werks   = _werks                    AND
           w_zsdt0082-bloqueio = abap_false.
          _dados-kwmeng            = _dados-kwmeng + _dados_0415-qte_sol.
          _dados-dt_entrega        = _dados_0415-dt_entrega.
          _dados-nr_rot_pc         = _dados_0415-nr_rot_pc.
          _dados-prioridade        = _dados_0415-prioridade.
          _dados-flexibilidade     = _dados_0415-flexibilidade.
          _dados-carga_auto        = _dados_0415-carga_auto.
          _dados-transf_no_fornecedor = _dados_0415-transf_no_fornecedor.
          _dados-nao_editar        = abap_true.
        ELSE.
          IF w_zsdt0082-bloqueio   = abap_true.
            IF _dados_0415-nro_sol <> w_zsdt0082-nro_sol_origem.
              lv_utilizado         = _dados_0415-qte_sol.
*             _dados-menge_consumo = _dados-menge_consumo + ( lv_utilizado - _dados-kwmeng ).
              _dados-clabs         = _dados-clabs - lv_utilizado.
            ENDIF.
          ELSE.
            lv_utilizado           = _dados_0415-qte_sol.
            _dados-menge_consumo   = _dados-menge_consumo + ( lv_utilizado - _dados-kwmeng ).
            _dados-clabs           = _dados-clabs - lv_utilizado.
          ENDIF.
        ENDIF.
      ENDLOOP.

*     lv_utilizado                 = lc_distribuicao_insumos->get_verificar_consumo( _dados ).
*     _dados-menge_consumo         = lv_utilizado - _dados-kwmeng.
*     _dados-clabs                 = _dados-clabs - lv_utilizado.
      MODIFY cg_dados           FROM _dados INDEX lv_tabix.
    ENDLOOP.

    DELETE cg_dados WHERE clabs = 0.
*-CS2025000249-16.06.2025-#182039-JT-fim

  ENDMETHOD.
ENDCLASS.
