CLASS ZCL_IM_LE_SHP_DELIVERY_PRO DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_IM_LE_SHP_DELIVERY_PRO
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES IF_BADI_INTERFACE .
    INTERFACES IF_EX_LE_SHP_DELIVERY_PROC .
  PROTECTED SECTION.
*"* protected components of class ZCL_IM_LE_SHP_DELIVERY_PRO
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_IM_LE_SHP_DELIVERY_PRO
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_LE_SHP_DELIVERY_PRO IMPLEMENTATION.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER.

  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~change_delivery_item.
    DATA: wa_vbuk TYPE vbukvb,
          sl_vbak TYPE vbak.

    DATA: vl_txt01(50) TYPE c,
          vl_txt02(50) TYPE c,
          vl_txt03(50) TYPE c,
          vl_txt04(50) TYPE c.


    READ TABLE it_xvbuk INTO wa_vbuk INDEX 1.


    CLEAR sl_vbak.
    SELECT SINGLE *
      FROM vbak
      INTO sl_vbak
      WHERE vbeln = wa_vbuk-vbeln.

    IF sy-tcode = 'VL01N' AND sl_vbak-zpesagem = '01'.
      vl_txt01 = 'Não é possivel fazer remessa de Ordem de venda'.
      vl_txt02 = 'com pesagem OPUS por essa transação.'.
      vl_txt03 = 'Para esse tipo de ordem utilizar a transação'.
      vl_txt04 = 'ZOPUS.'.
      MESSAGE e836(sd) WITH vl_txt01 vl_txt02 vl_txt03 vl_txt04.
    ENDIF.

    IF sy-tcode = 'VL01N' AND sl_vbak-zpesagem = '03'.
      vl_txt01 = 'Não é possivel fazer remessa de Ordem de venda'.
      vl_txt02 = 'com pesagem NF Terceiros por essa transação.'.
      vl_txt03 = 'Para esse tipo de ordem utilizar a transação'.
      vl_txt04 = 'ZSDT0018'.
      MESSAGE e836(sd) WITH vl_txt01 vl_txt02 vl_txt03 vl_txt04.
    ENDIF.

*  DATA: vl_cap_kg  TYPE zlest0002-cap_kg,
*        wa_vbuk    TYPE vbukvb,
*        sl_vbak    TYPE vbak,
*        sl_vbap    TYPE vbap.
*
*  TYPES: BEGIN OF type_txt,
*          field(2000) TYPE c,
*        END   OF type_txt.
*
*  DATA: tl_txt TYPE TABLE OF type_txt,
*        vl_tabix TYPE i              ,
*        sl_field TYPE type_txt       .
*
*  DATA: v_menge_i TYPE mdr1germenge,
*        v_menge_o TYPE f,
*        v_ok TYPE c.
*
*  DATA: gt_zsdt0001 TYPE TABLE OF zsdt0001.
*
*  DATA: ls_zsdt0001 TYPE zsdt0001.
*
*  DATA: v_coluna1(13) TYPE c,
*        v_coluna2(13) TYPE c,
*        v_coluna3(13) TYPE c,
*        v_coluna4(13) TYPE c,
*        v_coluna5(13) TYPE c,
*        v_coluna6(13) TYPE c,
*        v_coluna7(13) TYPE c,
*        v_coluna8(13) TYPE c,
*        v_coluna9(13) TYPE c.
*
*
*
*  DATA: v_coluna1t(13) TYPE c VALUE '            |',
*        v_coluna2t(13) TYPE c VALUE '            |',
*        v_coluna3t(13) TYPE c VALUE '            |',
*        v_coluna4t(13) TYPE c VALUE '            |',
*        v_coluna5t(13) TYPE c VALUE '            |',
*        v_coluna6t(13) TYPE c VALUE '            |',
*        v_coluna7t(13) TYPE c VALUE '            |',
*        v_coluna8t(13) TYPE c VALUE '            |',
*        v_coluna9t(13) TYPE c VALUE '            |'.
*
*  DATA: v_coluna1_aux(10) TYPE c,
*        v_coluna2_aux(10) TYPE c,
*        v_coluna3_aux(10) TYPE c,
*        v_coluna4_aux(10) TYPE c,
*        v_coluna5_aux(10) TYPE c,
*        v_coluna6_aux(10) TYPE c,
*        v_coluna7_aux(10) TYPE c,
*        v_coluna8_aux(10) TYPE c,
*        v_coluna9_aux(10) TYPE c.
*
*  READ TABLE it_xvbuk INTO wa_vbuk INDEX 1.
*
*  CLEAR sl_vbak.
*  SELECT SINGLE *
*    FROM vbak
*    INTO sl_vbak
*    WHERE vbeln = wa_vbuk-vbeln.
*
*  IF sy-tcode         = 'VL01N' AND sl_vbak-zpesagem = '01'.
*    SELECT SINGLE *
*      FROM vbap
*      INTO sl_vbap
*    WHERE  vbeln EQ sl_vbak-vbeln.
*    "Devolução de remessa de armazenagem, buscar romaneio de entrada e não filtrar pela ordem de venda
*    IF sl_vbak-auart EQ 'ZRAG'.
*      SELECT *
*         FROM zsdt0001
*         INTO TABLE gt_zsdt0001
*       WHERE bukrs    EQ sl_vbak-bukrs_vf
*         AND branch   EQ sl_vbap-werks
*         AND nr_safra EQ sl_vbap-charg
*         AND parid    EQ sl_vbak-kunnr
*         AND matnr    EQ sl_vbap-matnr
*         AND tp_movimento EQ 'E'
*         AND status   NE 'X'.
*    ELSE.
*      SELECT *
*        FROM zsdt0001
*        INTO TABLE gt_zsdt0001
*      WHERE vbeln  EQ cs_lips-vgbel
*        AND matnr  EQ sl_vbap-matnr
*        AND status NE 'X'.
*    ENDIF.
*
*    IF sy-subrc = 0.
*
*      OVERLAY v_coluna1t WITH 'Nr. Romaneio'.
*      OVERLAY v_coluna2t WITH 'Cliente     '.
*      OVERLAY v_coluna3t WITH 'Filial      '.
*      OVERLAY v_coluna4t WITH 'Placa       '.
*      OVERLAY v_coluna5t WITH 'Data        '.
*      OVERLAY v_coluna6t WITH 'Quantidade  '.
*      OVERLAY v_coluna7t WITH 'E/S         '.
*      OVERLAY v_coluna8t WITH 'Material    '.
*      OVERLAY v_coluna9t WITH 'Frete       '.
*      "Nr romaneio, Cliente, Filial, Placa, Data, Quantidade, E/S, Matnr (Material), Tipo de frete.
*      CONCATENATE      v_coluna1t
*                       v_coluna2t
*                       v_coluna3t
*                       v_coluna4t
*                       v_coluna5t
*                       v_coluna6t
*                       v_coluna7t
*                       v_coluna8t
*                       v_coluna9t
*                     INTO  sl_field-field.
*      APPEND sl_field TO tl_txt.
*
*      sl_field-field = sy-uline(117).
*      APPEND sl_field TO tl_txt.
*
*      LOOP AT gt_zsdt0001 INTO ls_zsdt0001.
*
*        WRITE ls_zsdt0001-nr_romaneio    TO v_coluna1_aux.
*        WRITE ls_zsdt0001-parid          TO v_coluna2_aux.
*        WRITE ls_zsdt0001-branch         TO v_coluna3_aux.
*        WRITE ls_zsdt0001-placa_cav      TO v_coluna4_aux.
*        WRITE ls_zsdt0001-dt_movimento   TO v_coluna5_aux.
*        WRITE ls_zsdt0001-peso_liq       TO v_coluna6_aux.
*        WRITE ls_zsdt0001-tp_movimento   TO v_coluna7_aux.
*        WRITE ls_zsdt0001-matnr          TO v_coluna8_aux.
*        WRITE ls_zsdt0001-tp_frete       TO v_coluna9_aux.
*
**      CLEAR: v_coluna1, v_coluna2, v_coluna3, v_coluna4, v_coluna5.
*        v_coluna1 = '            |'.
*        v_coluna2 = '            |'.
*        v_coluna3 = '            |'.
*        v_coluna4 = '            |'.
*        v_coluna5 = '            |'.
*        v_coluna6 = '            |'.
*        v_coluna7 = '            |'.
*        v_coluna8 = '            |'.
*        v_coluna9 = '            |'.
*
*
*        OVERLAY v_coluna1 WITH v_coluna1_aux.
*        OVERLAY v_coluna2 WITH v_coluna2_aux.
*        OVERLAY v_coluna3 WITH v_coluna3_aux.
*        OVERLAY v_coluna4 WITH v_coluna4_aux.
*        OVERLAY v_coluna5 WITH v_coluna5_aux.
*        OVERLAY v_coluna6 WITH v_coluna6_aux.
*        OVERLAY v_coluna7 WITH v_coluna7_aux.
*        OVERLAY v_coluna8 WITH v_coluna8_aux.
*        OVERLAY v_coluna9 WITH v_coluna9_aux.
*
*        CONCATENATE v_coluna1
*                    v_coluna2
*                    v_coluna3
*                    v_coluna4
*                    v_coluna5
*                    v_coluna6
*                    v_coluna7
*                    v_coluna8
*                    v_coluna9
*                    INTO  sl_field-field.
*        APPEND sl_field TO tl_txt.
*      ENDLOOP.
*
*      WHILE v_ok IS INITIAL.
*        CLEAR vl_tabix.
**
**        NEW-PAGE LINE-SIZE 200
**        LINE-COUNT 40.
*
*        CALL FUNCTION 'ZPOPUP_WITH_TABLE_DISPLAY'
*          EXPORTING
*            endpos_col   = 117
*            endpos_row   = 10
*            startpos_col = 1
*            startpos_row = 3
*            titletext    = 'Selecione o Romaneio a ser usado'
*            tamanho_area = 117
*          IMPORTING
*            choise       = vl_tabix
*          TABLES
*            valuetab     = tl_txt
*          EXCEPTIONS
*            break_off    = 1
*            OTHERS       = 2.
*        "Linha 4 é a linha da descrição da coluna
*        IF vl_tabix LT 4.
*          IF vl_tabix = 0.
*            LEAVE TO CURRENT TRANSACTION.
*          ENDIF.
*          MESSAGE i897(sd) WITH 'Favor selecionar um dado valido.'.
*        ENDIF.
*
*        IF vl_tabix GE 5.
*
*          vl_tabix = vl_tabix - 4.
*
*          READ TABLE gt_zsdt0001 INTO ls_zsdt0001 INDEX vl_tabix.
*          IF sy-subrc = 0.
*            "EXPORT ls_zsdt0001 FROM ls_zsdt0001 TO MEMORY ID 'ZSDQUANT'.
*            "campo utilizado para identificar o romaneio escolhido no momento de gravar
*            cs_lips-kdmat = ls_zsdt0001-ch_referencia.
*            cs_lips-lfimg = ls_zsdt0001-peso_liq.
*
*            v_menge_i = ls_zsdt0001-peso_liq.
*
*            CALL FUNCTION 'MD_CONV_QUANTITY_PACK_TO_FLOAT'
*              EXPORTING
*                iv_menge = v_menge_i
*              IMPORTING
*                ev_menge = v_menge_o.
*
*            cs_lips-lfimg_flo = v_menge_o.
*            cs_lips-lgmng_flo = v_menge_o.
*
*          ENDIF.
*
*
*          CLEAR vl_cap_kg.
*          SELECT SINGLE cap_kg
*            FROM zlest0002
*            INTO vl_cap_kg
*            WHERE pc_veiculo = ls_zsdt0001-placa_cav.
*          IF sy-subrc = 0.
*            IF ls_zsdt0001-peso_liq GT vl_cap_kg.
*              MESSAGE i897(sd) WITH 'Peso excedente à capacidade do veículo'.
*            ELSE.
*              v_ok = 'X'.
*            ENDIF.
*          ELSE.
*            v_ok = 'X'.
*          ENDIF.
*        ENDIF.
*
*      ENDWHILE.
*    ELSE.
*      MESSAGE e897(sd) WITH 'Não há pesagem no OPUS para esta remessa.'.
*    ENDIF.
*  ENDIF.

    DATA: sl_lips     TYPE lips,
          vl_centro_a TYPE werks_d,
          vl_clabs_a  TYPE labst,
          vl_clabs_f  TYPE labst,
          vl_total    TYPE labst,
          vl_aux      TYPE char18,
          vl_msn1     TYPE char50,
          vl_msn2     TYPE char50,
          tg_0023     TYPE TABLE OF zmm0023, " Novo ajuste
          wa_0023     TYPE zmm0023, " Novo ajuste
          wa_0023_aux TYPE sy-subrc, "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
          wa_zmmt0017 TYPE zmmt0017. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

    DATA: lva_charg_int TYPE i,
          lva_charg_a   TYPE zsdt0001-nr_safra,
          lva_clabs_a1  TYPE labst.

    IF sy-tcode EQ 'VL01N' OR
       sy-tcode EQ 'VL02N' OR
       sy-tcode EQ 'VL10B' .

      " Novo Ajuste

      SELECT *
      FROM zmm0023
      INTO TABLE tg_0023.

*      SORT TG_0023 BY  WERKS  ASCENDING MATNR ASCENDING CWERKS DESCENDING. "CS2016001304
      SORT tg_0023 BY  werks ASCENDING matnr ASCENDING matkl ASCENDING cwerks DESCENDING."PBALVES

      " Exclui os registros antigos e pega somente o recente.
      "delete adjacent duplicates from tg_0023 comparing werks. CS2016001304

      "Fim Novo Ajuste


      READ TABLE it_xlips INTO sl_lips INDEX 1.
      CHECK NOT sl_lips IS INITIAL.

***********************************************************************
*comentado em 24.04.2024 para subir request Samuel
** PSA CONVERT MATNR 18
**
**     data aux_matnr    TYPE matnr18.
**
**** Formata o código do material
**  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
**    EXPORTING
**      input  = sl_lips-matnr "campo de 400char
**    IMPORTING
**      output = aux_matnr.
**
**      CLEAR: sl_lips-matnr.
**
**  sl_lips-matnr = aux_matnr.
**
**  clear: aux_matnr.
**
** END CONVERT
***********************************************************************


*    read table tg_0023 into wa_0023 with key werks = sl_lips-werks status = 'A'. " Novo ajuste
      CLEAR wa_0023.
      READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = sl_lips-werks
                                                     matnr = sl_lips-matnr. "lê o primeiro CS2023000120 Urgente - Atualização tela de bloqueio
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria PSA
      IF sy-subrc NE 0.
        SELECT SINGLE matkl INTO @DATA(_matkl) FROM mara WHERE matnr = @sl_lips-matnr.
        READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = sl_lips-werks
                                                 matkl = _matkl.
      ENDIF.
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
      CLEAR wa_0023_aux.
      IF
        sy-subrc NE 0.
        wa_0023_aux = sy-subrc.
      ENDIF.
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

      IF NOT sy-subrc = 0 OR wa_0023-status NE 'A'.

*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*        SELECT SINGLE CENTRO_A_FIXAR
*          FROM ZMMT0017
*          INTO VL_CENTRO_A
*        WHERE  MATNR       EQ SL_LIPS-MATNR
*          AND  CENTRO_FIXO EQ SL_LIPS-WERKS
*          AND  LGORT       EQ SL_LIPS-LGORT.


        zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
                 EXPORTING
                   i_material        = sl_lips-matnr
                   i_centro_fixo     = sl_lips-werks
                   i_deposito        = sl_lips-lgort
                 IMPORTING
                   e_single_depara   = wa_zmmt0017
               ).

*Parâmetros Centr

        IF wa_zmmt0017 IS  NOT INITIAL.
          vl_centro_a = wa_zmmt0017-centro_a_fixar.
          "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - fim
          "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
          IF wa_0023_aux IS NOT INITIAL.
            MESSAGE e897(sd) WITH  'Falta parâmetros na ZMM0029. '
                                      'Favor entrar em contato com '
                                       'a área de controladoria e estoque. '.
          ENDIF.
          "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

          DATA:  vl_pikmg(40).

          FIELD-SYMBOLS: <fs_pikmg> TYPE any.

          vl_pikmg = '(SAPMV50A)LIPSD-PIKMG'.
          ASSIGN (vl_pikmg) TO <fs_pikmg>.

          SELECT SINGLE clabs
              FROM mchb
              INTO vl_clabs_f
            WHERE  matnr EQ sl_lips-matnr
              AND  werks EQ sl_lips-werks
              AND  lgort EQ sl_lips-lgort
              AND  charg EQ sl_lips-charg.

          CLEAR: lva_clabs_a1.

          IF vl_centro_a IS NOT INITIAL.

            IF strlen( sl_lips-charg ) = 4.
              TRY.
                  lva_charg_int = sl_lips-charg.
                  lva_charg_a   = sl_lips-charg.

                  IF lva_charg_int GT 2019.
                    CONCATENATE sl_lips-charg '_' sl_lips-werks INTO lva_charg_a.

                    SELECT SINGLE clabs
                      FROM mchb
                      INTO lva_clabs_a1
                    WHERE  matnr EQ sl_lips-matnr
                      AND  werks EQ vl_centro_a
                      AND  lgort EQ sl_lips-lgort
                      AND  charg EQ lva_charg_a.
                  ENDIF.
                CATCH cx_sy_conversion_no_number.
              ENDTRY.
            ENDIF.

            SELECT SINGLE clabs
              FROM mchb
              INTO vl_clabs_a
            WHERE  matnr EQ sl_lips-matnr
              AND  werks EQ vl_centro_a
              AND  lgort EQ sl_lips-lgort
              AND  charg EQ sl_lips-charg.
          ENDIF.

          vl_total = vl_clabs_a + vl_clabs_f + lva_clabs_a1.
          "IF SL_LIPS-LFIMG GT VL_TOTAL.
          IF <fs_pikmg> GT vl_total.
            vl_aux = vl_total.
            CONDENSE vl_aux NO-GAPS.
            CONCATENATE 'O total'
                        vl_aux
                        'do centro'
                   INTO vl_msn1 SEPARATED BY space.
            CONCATENATE sl_lips-werks
                        'e'
                        vl_centro_a
                   INTO vl_msn2 SEPARATED BY space.
            MESSAGE e897(sd) WITH vl_msn1
                                  vl_msn2
                                  'é menor que a quantidade do picking'.
          ENDIF.
          CLEAR: vl_pikmg.
        ENDIF.
      ENDIF. " Fim Nova alteração
    ENDIF.
  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES.

    CHECK 1 = 1.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES.

    DATA: V_FIELD_PESO(30) TYPE C,
          V_FIELD1(30)     TYPE C,
          V_FIELD2(30)     TYPE C,
          V_PESO           TYPE ZSDT0001-PESO_LIQ,
          V_PESO1          TYPE ZSDT0001-PESO_LIQ,
          V_PESO2          TYPE ZSDT0001-PESO_LIQ,
          WA_LIPS          TYPE LIPSVB,
          V_EXIT           TYPE C,
          WA_VBUK          TYPE VBUKVB,
          SL_VBAK          TYPE VBAK,
          SL_VBAP          TYPE VBAP,
          V_EDITAR(1)      TYPE C.

    DATA: WA_FIELD_ATTRIBUTES TYPE SHP_SCREEN_ATTRIBUTES.

    DATA: LT_XVBUK TYPE SHP_VL10_VBUK_T.

    DATA: V_X TYPE C.

    FIELD-SYMBOLS <FS_PESO>  TYPE ZSDT0001-PESO_LIQ.
    FIELD-SYMBOLS <FS_PESO1> TYPE ZSDT0001-PESO_LIQ.
    FIELD-SYMBOLS <FS_PESO2> TYPE ZSDT0001-PESO_LIQ.

    IF ( SY-TCODE(4) = 'VL01' ) OR ( SY-TCODE(4) = 'VL02' ).
      "Deleção de registro onde vbeln é nulo porque existe situação onde a tabela interna vem com dois registros,
      "mas somente um possui o vbeln
      LT_XVBUK[] = IT_XVBUK[].
      DELETE LT_XVBUK WHERE VBELN IS INITIAL.
      READ TABLE LT_XVBUK  INTO WA_VBUK INDEX 1.

      CLEAR SL_VBAK.
      SELECT SINGLE *
        FROM VBAK
        INTO SL_VBAK
        WHERE VBELN = WA_VBUK-VBELN.
      READ TABLE IT_XLIPS INTO WA_LIPS INDEX 1.
      "Se for transferencia verificar se a variavel de memoria que indica se pode editar
      IF WA_LIPS-VGTYP EQ 'V'.
        "Importa da função Z_SD_UPDATE_PICK método
        IMPORT V_EDITAR TO V_EDITAR FROM MEMORY ID 'ZEDITAR'.
      ENDIF.

      "Bloquear os campos se for pesagem OPUS identificado pela ordem de venda ou se for uma transferencia
      IF SL_VBAK-ZPESAGEM = '01' OR ( WA_LIPS-VGTYP EQ 'V' AND V_EDITAR IS INITIAL ).
        V_FIELD1 = '(SAPMV50A)LIPSD-G_LFIMG'.
        ASSIGN (V_FIELD1) TO <FS_PESO1>.
        IF SY-SUBRC = 0.
          V_PESO1 = <FS_PESO1>.
        ELSE.
          UNASSIGN <FS_PESO1>.
          CLEAR V_PESO1.
        ENDIF.

        V_FIELD2 = '(SAPMV50A)LIPSD-PIKMG'.
        ASSIGN (V_FIELD2) TO <FS_PESO2>.
        IF SY-SUBRC = 0.
          V_PESO2 = <FS_PESO2>.
        ELSE.
          UNASSIGN <FS_PESO2>.
          CLEAR V_PESO2.
        ENDIF.

        V_FIELD_PESO = '(SAPMV50A)LIPS-LFIMG'.
        ASSIGN (V_FIELD_PESO) TO <FS_PESO>.
        IF SY-SUBRC = 0.
          V_PESO = <FS_PESO>.
        ELSE.
          UNASSIGN <FS_PESO>.
          CLEAR V_PESO.
        ENDIF.

        <FS_PESO1> =  V_PESO.
        <FS_PESO2> =  V_PESO.

        MOVE 'LIPSD-G_LFIMG' TO WA_FIELD_ATTRIBUTES-NAME.
        MOVE  '0' TO WA_FIELD_ATTRIBUTES-INPUT.
        APPEND WA_FIELD_ATTRIBUTES TO CT_FIELD_ATTRIBUTES.

        MOVE 'LIPSD-PIKMG' TO WA_FIELD_ATTRIBUTES-NAME.
        MOVE  '0' TO WA_FIELD_ATTRIBUTES-INPUT.
        APPEND WA_FIELD_ATTRIBUTES TO CT_FIELD_ATTRIBUTES.

        MOVE 'LIPS-BRGEW' TO WA_FIELD_ATTRIBUTES-NAME.
        MOVE  '0' TO WA_FIELD_ATTRIBUTES-INPUT.
        APPEND WA_FIELD_ATTRIBUTES TO CT_FIELD_ATTRIBUTES.

        MOVE 'LIPS-LFIMG' TO WA_FIELD_ATTRIBUTES-NAME.
        MOVE  '0' TO WA_FIELD_ATTRIBUTES-INPUT.
        APPEND WA_FIELD_ATTRIBUTES TO CT_FIELD_ATTRIBUTES.

        MOVE 'LIPS-NTGEW' TO WA_FIELD_ATTRIBUTES-NAME.
        MOVE  '0' TO WA_FIELD_ATTRIBUTES-INPUT.
        APPEND WA_FIELD_ATTRIBUTES TO CT_FIELD_ATTRIBUTES.

        MOVE 'LIPS-NTGEW' TO WA_FIELD_ATTRIBUTES-NAME.
        MOVE  '0' TO WA_FIELD_ATTRIBUTES-INPUT.
        APPEND WA_FIELD_ATTRIBUTES TO CT_FIELD_ATTRIBUTES.

        MOVE 'LIPS-GEWEI' TO WA_FIELD_ATTRIBUTES-NAME.
        MOVE  '0' TO WA_FIELD_ATTRIBUTES-INPUT.
        APPEND WA_FIELD_ATTRIBUTES TO CT_FIELD_ATTRIBUTES.


      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~check_item_deletion.

    DATA: v_id              TYPE  thead-tdid,
          v_language        TYPE  thead-tdspras,
          v_name            TYPE  thead-tdname,
          wa_lips           TYPE lipsvb,
          v_romaneio        TYPE znr_romaneio,
          wa_zsdt0001       TYPE zsdt0001,
          wa_zsdt0023       TYPE zsdt0023,
          wa_lines          TYPE  tline,
          lt_zdco_vinculo   TYPE TABLE OF zdco_vinculo,
          v_object          TYPE  thead-tdobject,
          z_auart           TYPE  vbak-auart,
          is_block          TYPE char01,
          vl_index          TYPE sy-index,
          return            TYPE TABLE OF bapiret2,
          wa_return         TYPE  bapiret2,
          returnrollback    TYPE TABLE OF  bapiret2,
          wa_returnrollback TYPE  bapiret2.

*** Modificação - Eduardo Ruttkowski Tavares - 19.08.2013 >>> INI
    DATA: wa_vinculo  TYPE zdco_vinculo,
          wa_produtor TYPE zdco_produtor.
    CLEAR: wa_vinculo, wa_produtor.
*** Modificação - Eduardo Ruttkowski Tavares - 19.08.2013 <<< FIM

    DATA: lt_lines TYPE TABLE OF tline.

    READ TABLE it_xlips INTO wa_lips INDEX 1.

    IF ( sy-tcode(4) EQ 'VL02' OR sy-tcode(4) EQ 'VL03' OR sy-tcode EQ 'ZLES0106' OR sy-tcode EQ 'ZMM0127' OR sy-tcode = 'ZLES0136' OR sy-batch EQ abap_true ).
      CLEAR: v_id,
             v_language,
             v_name,
             v_object.

*** Modificação - Eduardo Ruttkowski Tavares - 19.08.2013 >>> INI

      SELECT SINGLE * FROM zdco_vinculo INTO wa_vinculo
        WHERE vbeln = is_likp-vbeln.

      IF sy-subrc = 0.

        DELETE FROM zdco_vinculo WHERE vbeln = is_likp-vbeln.

        SELECT SINGLE * FROM zdco_produtor INTO wa_produtor
          WHERE nu_dco = wa_vinculo-nu_dco AND
                nr_dco = wa_vinculo-nr_dco.

        IF sy-subrc = 0.
          wa_produtor-qt_remessa = wa_produtor-qt_remessa - wa_vinculo-qt_vinculada.
          MODIFY zdco_produtor FROM wa_produtor.
        ENDIF.

      ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 19.08.2013 <<< FIM

      v_object = 'VBBK'.
      v_name   = wa_lips-vbeln.
      v_id     = '0001'.
      v_language  = 'P'.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = v_id
          language                = v_language
          name                    = v_name
          object                  = v_object
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      READ TABLE lt_lines INTO wa_lines INDEX 2.
      IF sy-subrc = 0.

        v_romaneio = wa_lines-tdline+20(9).

        SELECT SINGLE *
          FROM zsdt0023
          INTO wa_zsdt0023
          WHERE vbeln EQ wa_lips-vbeln.

        IF sy-subrc IS INITIAL.

          CLEAR: vl_index, return, wa_return, is_block.
          IF wa_zsdt0023-mblnr_e IS NOT INITIAL.

*           "// Verifica se o documento esta estornado
            SELECT COUNT(*)
              FROM mseg
              WHERE smbln EQ wa_zsdt0023-mblnr_e.

            IF sy-subrc IS NOT INITIAL.

              DO 5 TIMES.

                vl_index = sy-index.

                CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
                  EXPORTING
                    materialdocument = wa_zsdt0023-mblnr_e
                    matdocumentyear  = wa_zsdt0023-mjahr_e
                  TABLES
                    return           = return.

                READ TABLE return INTO wa_return WITH KEY type = 'E'.
                IF sy-subrc IS NOT INITIAL.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = abap_true.
                  EXIT.
                ELSE.

                  CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
                    EXPORTING
                      id       = wa_return-id
                      number   = wa_return-number
                    IMPORTING
                      is_block = is_block.

                  IF is_block IS NOT INITIAL AND vl_index NE 5.
                    WAIT UP TO 2 SECONDS.
                  ELSE.

                    APPEND VALUE #(
                                    vbeln = wa_lips-vbeln
                                    posnr = wa_lips-abelp
                                    msgty = wa_return-type
                                    msgid = wa_return-id
                                    msgno = wa_return-number
                                    msgv1 = wa_return-message_v1
                                    msgv2 = wa_return-message_v2
                                    msgv3 = wa_return-message_v3
                                    msgv4 = wa_return-message_v4
                                  ) TO ct_log.

                    cf_item_not_deletable = abap_true.
                    EXIT.

                  ENDIF.
                ENDIF.
              ENDDO.
            ENDIF.
          ENDIF.

          CHECK cf_item_not_deletable IS INITIAL.

          CLEAR: vl_index, return, wa_return, is_block.
          IF wa_zsdt0023-mblnr_s IS NOT INITIAL.

*           "// Verifica se o documento esta estornado
            SELECT COUNT(*)
              FROM mseg
              WHERE smbln EQ wa_zsdt0023-mblnr_s.

            IF sy-subrc IS NOT INITIAL.

              DO 5 TIMES.

                vl_index = sy-index.

                CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
                  EXPORTING
                    materialdocument = wa_zsdt0023-mblnr_s
                    matdocumentyear  = wa_zsdt0023-mjahr_s
                  TABLES
                    return           = return.

                READ TABLE return INTO wa_return WITH KEY type = 'E'.
                IF sy-subrc IS NOT INITIAL.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = abap_true.
                  EXIT.
                ELSE.

                  CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
                    EXPORTING
                      id       = wa_return-id
                      number   = wa_return-number
                    IMPORTING
                      is_block = is_block.

                  IF is_block IS NOT INITIAL AND vl_index NE 5.
                    WAIT UP TO 2 SECONDS.
                  ELSE.

                    APPEND VALUE #(
                                    vbeln = wa_lips-vbeln
                                    posnr = wa_lips-abelp
                                    msgty = wa_return-type
                                    msgid = wa_return-id
                                    msgno = wa_return-number
                                    msgv1 = wa_return-message_v1
                                    msgv2 = wa_return-message_v2
                                    msgv3 = wa_return-message_v3
                                    msgv4 = wa_return-message_v4
                                  ) TO ct_log.

                    cf_item_not_deletable = abap_true.
                    EXIT.

                  ENDIF.
                ENDIF.
              ENDDO.
            ENDIF.
          ENDIF.

          CHECK cf_item_not_deletable IS INITIAL.

        ENDIF.

        CLEAR wa_zsdt0001.

        SELECT SINGLE *
         FROM zsdt0001
         INTO wa_zsdt0001
         WHERE nr_romaneio = v_romaneio
           AND doc_rem = wa_lips-vbeln
           AND status      = 'X'.

        IF sy-subrc IS INITIAL.

          CLEAR wa_zsdt0001-status.
          CLEAR wa_zsdt0001-doc_rem.

          MODIFY zsdt0001 FROM wa_zsdt0001.

          CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
            EXPORTING
              cd_referencia = wa_zsdt0001-ch_referencia
              tp_bloqueio   = space.

        ENDIF.
      ENDIF.

*   NF Terceiros
      DELETE FROM zsdt0009 WHERE vbeln_vl EQ wa_lips-vbeln.

* Clear Remessa da tabela 53 do processo da Cockipt zsdt0066
      SELECT SINGLE COUNT(*)
        FROM zsdt0053
        WHERE remessa_exp EQ is_likp-vbeln.

      IF sy-subrc IS INITIAL.
        UPDATE zsdt0053 SET remessa_exp = abap_false
                      WHERE remessa_exp EQ is_likp-vbeln.
      ENDIF.
    ENDIF.

*  DATA: wa_lips         TYPE lipsvb,
*        lt_zdco_vinculo TYPE TABLE OF zdco_vinculo,
*        z_auart         TYPE  vbak-auart,
*        WA_CT_LOG       TYPE SHP_BADI_ERROR_LOG  .
*
*  read table it_xlips into wa_lips index 1.
*
*  IF ( sy-tcode(4) EQ 'VL02' OR sy-tcode(4) EQ 'VL03' ).
*
**--Validação do DCO
*
*    SELECT SINGLE auart
*      INTO z_auart
*      FROM vbak
*      WHERE vbeln EQ wa_lips-vgbel.
*
*    IF z_auart = 'ZRDC' .
*      SELECT *
*        INTO TABLE lt_zdco_vinculo
*        FROM zdco_vinculo
*        WHERE vbeln EQ wa_lips-vbeln .
*
*      IF sy-subrc IS INITIAL.
*
*        MESSAGE 'Desvincular o DCO antes de eliminar a Remessa!' TYPE 'I'.
*
*        CF_ITEM_NOT_DELETABLE = 'X'.
*
*      ENDIF.
*    ENDIF.
*  endif.

    "MESSAGE 'Desvincular o DCO antes de eliminar a Remessa!' TYPE 'E'.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION.

    "MESSAGE 'Desvincular o DCO antes de eliminar a Remessa!' TYPE 'E'.

  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~delivery_final_check.

    DATA: wa_xlips   TYPE lipsvb.

    IF ( sy-ucomm = 'WABU_T' ) OR ( sy-ucomm IS INITIAL ).
      LOOP AT it_xlips INTO wa_xlips.

        CALL FUNCTION 'Z_VAL_VOLUME_LANC_ALGODAO'
          EXPORTING
            p_matnr    = wa_xlips-matnr
            p_volume   = wa_xlips-volum
          EXCEPTIONS
            inf_volume = 1
            OTHERS     = 2.

        IF NOT sy-subrc IS INITIAL.
          CLEAR: sy-ucomm.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDLOOP.



    ENDIF.

* Inicio BCI - Campos obligatorios en entrega
    DATA:  ls_finchdel      TYPE finchdel,
           lv_mostrar TYPE char1.

    IF sy-ucomm = 'SICH_T' or sy-ucomm = 'WABU_T'.

      READ TABLE it_xlikp
          INTO DATA(ls_likp)
          INDEX 1.

*      IF ls_likp-lfart EQ 'ZLF' AND
*         ( ls_likp-vstel EQ 'F115' OR
*           ls_likp-vstel EQ 'F125' OR
*           ls_likp-vstel EQ 'F135').

        CALL FUNCTION 'ZDELIV_HEAD_SCREEN_VAL_ENABLE'
          EXPORTING
            im_lfart   = ls_likp-lfart
            im_vstel   = ls_likp-vstel
            im_werks   = ls_likp-werks
          IMPORTING
            ex_mostrar = lv_mostrar.

        IF lv_mostrar EQ 'X'.

          IF ls_likp-anzpk IS INITIAL.

            CLEAR ls_finchdel.
            ls_finchdel-vbeln    = ls_likp-vbeln.
            ls_finchdel-pruefung = '99'.
            ls_finchdel-msgty    = 'E'.
            ls_finchdel-msgid    = 'FB'.
            ls_finchdel-msgno    = '000'.
            ls_finchdel-msgv1 = TEXT-001.

            INSERT ls_finchdel INTO TABLE ct_finchdel.
            RETURN.

          ENDIF.

*        ENDIF.

      ENDIF.

    ENDIF.
* Fin BCI - Campos obligatorios en entrega

* Inicio BCI - Campos obligatorios COT
*    IF sy-ucomm = 'WABU_T'. "Contabilizar SM
*
*      DATA: lv_mostrar TYPE char1,
*            lv_msg     TYPE bapi_msg.
*
*      READ TABLE it_xlikp
*          INTO ls_likp
*          INDEX 1.
*
*      CALL FUNCTION 'ZDELIV_HEAD_SCREEN_VAL_ENABLE'
*        EXPORTING
*          im_lfart   = ls_likp-lfart
*          im_vstel   = ls_likp-vstel
*          im_werks   = ls_likp-werks
*        IMPORTING
*          ex_mostrar = lv_mostrar.
*
*      IF lv_mostrar EQ 'X'.
*
*        CALL FUNCTION 'ZSD_DELIV_HEAD_CUST_VALIDATE'
*          IMPORTING
*            ex_message = lv_msg.
*
*        IF lv_msg IS NOT INITIAL.
*
*          CLEAR ls_finchdel.
*          ls_finchdel-vbeln    = ls_likp-vbeln.
*          ls_finchdel-pruefung = '99'.
*          ls_finchdel-msgty    = 'E'.
*          ls_finchdel-msgid    = 'FB'.
*          ls_finchdel-msgno    = '000'.
*          ls_finchdel-msgv1 = lv_msg.
**     Note: CT_FINCHDEL is a hashed table
*          INSERT ls_finchdel INTO TABLE ct_finchdel.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH.

    CHECK 1 = 1.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER.

    CHECK 1 = 1.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM.

    DATA: VL_ADRNR   TYPE LFA1-ADRNR,
          VL_VSBED   TYPE LFM1-VSBED,
          VL_TRPZONO TYPE ADRC-TRANSPZONE,
          VL_COUNTRO TYPE ADRC-COUNTRY,
          VL_TRPZOND TYPE ADRC-TRANSPZONE,
          VL_COUNTRD TYPE ADRC-COUNTRY,
          VL_TRAGR   TYPE MARA-TRAGR,
          VL_ROUTE   TYPE TROLZ-ROUTE,
          VL_CAMPO   TYPE CHAR30.

    FIELD-SYMBOLS <ROUTE> TYPE LIKP-ROUTE.

    IF SY-TCODE EQ 'VL31N'.

      IF IS_LIKP-LFART EQ 'EL' AND
         IS_LIKP-ROUTE IS INITIAL.

        SELECT SINGLE ADRNR
          FROM LFA1
          INTO VL_ADRNR
        WHERE  LIFNR EQ IS_LIKP-LIFNR.

        IF SY-SUBRC IS INITIAL.
          SELECT SINGLE VSBED
            FROM LFM1
            INTO VL_VSBED
          WHERE  LIFNR EQ IS_LIKP-LIFNR.

          SELECT SINGLE TRANSPZONE COUNTRY
            FROM ADRC
            INTO (VL_TRPZONO, VL_COUNTRO)
          WHERE ADDRNUMBER EQ VL_ADRNR.
        ENDIF.

        IF NOT VL_TRPZONO IS INITIAL.
          CLEAR VL_ADRNR.
          SELECT SINGLE ADRNR
            FROM T001W
            INTO VL_ADRNR
          WHERE WERKS EQ CS_LIPS-WERKS.
        ENDIF.

        IF NOT VL_ADRNR IS INITIAL.
          SELECT SINGLE TRANSPZONE COUNTRY
            FROM ADRC
            INTO (VL_TRPZOND, VL_COUNTRD)
          WHERE ADDRNUMBER EQ VL_ADRNR.

          SELECT SINGLE TRAGR
            FROM MARA
            INTO VL_TRAGR
          WHERE MATNR EQ CS_LIPS-MATNR.

          SELECT SINGLE ROUTE
            FROM TROLZ
            INTO VL_ROUTE
          WHERE  ALAND EQ VL_COUNTRO
            AND  AZONE EQ VL_TRPZONO
            AND  VSBED EQ VL_VSBED
            AND  TRAGR EQ VL_TRAGR
            AND  LLAND EQ VL_COUNTRD
            AND  LZONE EQ VL_TRPZOND.
        ENDIF.

        IF NOT VL_ROUTE IS INITIAL.
          VL_CAMPO = '(SAPLV50S)LIKP-ROUTE'.
          ASSIGN  (VL_CAMPO) TO <ROUTE>.
          IF <ROUTE> IS ASSIGNED.
            <ROUTE> = VL_ROUTE.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~item_deletion.



    DATA: v_id              TYPE  thead-tdid,
          v_language        TYPE  thead-tdspras,
          v_name            TYPE  thead-tdname,
          wa_lips           TYPE lipsvb,
          v_romaneio        TYPE znr_romaneio,
          wa_zsdt0001       TYPE zsdt0001,
          wa_zsdt0023       TYPE zsdt0023,
          wa_lines          TYPE  tline,
          lt_zdco_vinculo   TYPE TABLE OF zdco_vinculo,
          v_object          TYPE  thead-tdobject,
          z_auart           TYPE  vbak-auart,
          is_block          TYPE char01,
          vl_index          TYPE sy-tabix,
          return            TYPE TABLE OF bapiret2,
          wa_return         TYPE  bapiret2,
          returnrollback    TYPE TABLE OF  bapiret2,
          wa_returnrollback TYPE  bapiret2.

*** Modificação - Eduardo Ruttkowski Tavares - 19.08.2013 >>> INI
    DATA: wa_vinculo  TYPE zdco_vinculo,
          wa_produtor TYPE zdco_produtor.
    CLEAR: wa_vinculo, wa_produtor.
*** Modificação - Eduardo Ruttkowski Tavares - 19.08.2013 <<< FIM

    DATA: lt_lines TYPE TABLE OF tline.

    READ TABLE it_xlips INTO wa_lips INDEX 1.

    IF ( sy-tcode(4) EQ 'VL02' OR sy-tcode(4) EQ 'VL03' OR sy-tcode EQ 'ZLES0106' OR sy-tcode EQ 'ZMM0127' OR sy-tcode = 'ZLES0136' OR sy-batch EQ abap_true ).
      CLEAR: v_id,
             v_language,
             v_name,
             v_object.


*** Modificação - Eduardo Ruttkowski Tavares - 19.08.2013 >>> INI

      SELECT SINGLE * FROM zdco_vinculo INTO wa_vinculo
        WHERE vbeln = is_likp-vbeln.

      IF sy-subrc = 0.

        DELETE FROM zdco_vinculo WHERE vbeln = is_likp-vbeln.

        SELECT SINGLE * FROM zdco_produtor INTO wa_produtor
          WHERE nu_dco = wa_vinculo-nu_dco AND
                nr_dco = wa_vinculo-nr_dco.

        IF sy-subrc = 0.
          wa_produtor-qt_remessa = wa_produtor-qt_remessa - wa_vinculo-qt_vinculada.
          MODIFY zdco_produtor FROM wa_produtor.
        ENDIF.

      ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 19.08.2013 <<< FIM
*--Validação do DCO

*    SELECT SINGLE auart
*      INTO z_auart
*      FROM vbak
*      WHERE vbeln EQ wa_lips-vgbel.
*
*    IF z_auart = 'ZRDC' .
*      SELECT *
*        INTO TABLE lt_zdco_vinculo
*        FROM zdco_vinculo
*        WHERE vbeln EQ wa_lips-vbeln .
*
*      IF sy-subrc IS INITIAL.
*        MESSAGE 'Desvincular o DCO antes de eliminar a Remessa!' TYPE 'I'.
*
*        CF_ITEM_NOT_DELETABLE = 'X'.
*
*        "EXCEPTIONS .
*
*        "MESSAGE 'Desvincular o DCO antes de eliminar a Remessa!' TYPE 'E'.
*      ENDIF.
*    ENDIF.

*--------------------

      v_object = 'VBBK'.
      v_name   = wa_lips-vbeln.
      v_id     = '0001'.
      v_language  = 'P'.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = v_id
          language                = v_language
          name                    = v_name
          object                  = v_object
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      READ TABLE lt_lines INTO wa_lines INDEX 2.
      IF sy-subrc = 0.

        v_romaneio = wa_lines-tdline+20(9).

        SELECT SINGLE *
          FROM zsdt0023
          INTO wa_zsdt0023
          WHERE vbeln EQ wa_lips-vbeln.

        IF sy-subrc IS INITIAL.

          CLEAR: vl_index, return, wa_return, is_block.
          IF wa_zsdt0023-mblnr_e IS NOT INITIAL.

*           "// Verifica se o documento esta estornado
            SELECT COUNT(*)
              FROM mseg
              WHERE smbln EQ wa_zsdt0023-mblnr_e.

            IF sy-subrc IS NOT INITIAL.

              DO 5 TIMES.

                vl_index = sy-tabix.

                CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
                  EXPORTING
                    materialdocument = wa_zsdt0023-mblnr_e
                    matdocumentyear  = wa_zsdt0023-mjahr_e
                  TABLES
                    return           = return.

                READ TABLE return INTO wa_return WITH KEY type = 'E'.
                IF sy-subrc IS NOT INITIAL.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = abap_true.
                ELSE.

                  CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
                    EXPORTING
                      id       = wa_return-id
                      number   = wa_return-number
                    IMPORTING
                      is_block = is_block.

                  IF is_block IS NOT INITIAL AND vl_index NE 5.
                    WAIT UP TO 2 SECONDS.
                  ELSE.
                    MESSAGE
                      ID wa_return-id
                      TYPE wa_return-type
                      NUMBER wa_return-number
                      WITH wa_return-message_v1
                           wa_return-message_v2
                           wa_return-message_v3
                           wa_return-message_v4.
                  ENDIF.
                ENDIF.
              ENDDO.
            ENDIF.
          ENDIF.

          CLEAR: vl_index, return, wa_return, is_block.
          IF wa_zsdt0023-mblnr_s IS NOT INITIAL.

*           "// Verifica se o documento esta estornado
            SELECT COUNT(*)
              FROM mseg
              WHERE smbln EQ wa_zsdt0023-mblnr_s.

            IF sy-subrc IS NOT INITIAL.

              DO 5 TIMES.

                vl_index = sy-tabix.

                CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
                  EXPORTING
                    materialdocument = wa_zsdt0023-mblnr_s
                    matdocumentyear  = wa_zsdt0023-mjahr_s
                  TABLES
                    return           = return.

                READ TABLE return INTO wa_return WITH KEY type = 'E'.
                IF sy-subrc IS NOT INITIAL.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = abap_true.
                ELSE.

                  CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
                    EXPORTING
                      id       = wa_return-id
                      number   = wa_return-number
                    IMPORTING
                      is_block = is_block.

                  IF is_block IS NOT INITIAL AND vl_index NE 5.
                    WAIT UP TO 2 SECONDS.
                  ELSE.
                    MESSAGE
                      ID wa_return-id
                      TYPE wa_return-type
                      NUMBER wa_return-number
                      WITH wa_return-message_v1
                           wa_return-message_v2
                           wa_return-message_v3
                           wa_return-message_v4.
                  ENDIF.
                ENDIF.
              ENDDO.
            ENDIF.
          ENDIF.
        ENDIF.

*      IF ( NOT wa_zsdt0023-mblnr_e IS INITIAL ).
*
*        CLEAR: return, wa_return.
*
*        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*          EXPORTING
*            materialdocument = wa_zsdt0023-mblnr_e
*            matdocumentyear  = wa_zsdt0023-mjahr_e
*          TABLES
*            return           = return.
*
*        READ TABLE return INTO wa_return WITH KEY type = 'E'.
*
*        IF NOT sy-subrc IS INITIAL.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              wait = 'X'.
*
*
*          IF ( NOT wa_zsdt0023-mblnr_s IS INITIAL ).
*
*            CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*              EXPORTING
*                materialdocument = wa_zsdt0023-mblnr_s
*                matdocumentyear  = wa_zsdt0023-mjahr_s
*              TABLES
*                return           = return.
*
*            READ TABLE return INTO wa_return WITH KEY type = 'E'.
*
*            IF NOT sy-subrc IS INITIAL.
*              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                EXPORTING
*                  wait = 'X'.
**            else.
**              call function 'BAPI_TRANSACTION_ROLLBACK'
**                importing
**                  return = returnrollback.
**
**              read table returnrollback into wa_returnrollback index 1.
**
**              if sy-subrc is initial.
**                message e000(z01) with 'Os documentos de movimentação de mercadorias foram estornados.' .
**              endif.
*
*
*
*            ENDIF.
*          ENDIF.
*
**        else.
**          call function 'BAPI_TRANSACTION_ROLLBACK'
**            importing
**              return = returnrollback.
**
**          read table returnrollback into wa_returnrollback index 1.
**
**          if sy-subrc is initial.
**            message e000(z01) with 'Os documentos de movimentação de mercadorias foram estornados.' .
**          endif.
*
*
*        ENDIF.
*
*      ENDIF.

        CLEAR wa_zsdt0001.

        SELECT SINGLE *
         FROM zsdt0001
         INTO wa_zsdt0001
         WHERE nr_romaneio = v_romaneio
           AND doc_rem = wa_lips-vbeln
           AND status      = 'X'.

        IF sy-subrc IS INITIAL.

          CLEAR wa_zsdt0001-status.
          CLEAR wa_zsdt0001-doc_rem.

          MODIFY zsdt0001 FROM wa_zsdt0001.

          CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
            EXPORTING
              cd_referencia = wa_zsdt0001-ch_referencia
              tp_bloqueio   = space.

        ENDIF.
      ENDIF.

*   NF Terceiros
      DELETE FROM zsdt0009 WHERE vbeln_vl EQ wa_lips-vbeln.

* Clear Remessa da tabela 53 do processo da Cockipt zsdt0066
      SELECT SINGLE COUNT(*)
        FROM zsdt0053
        WHERE remessa_exp EQ is_likp-vbeln.

      IF sy-subrc IS INITIAL.
        UPDATE zsdt0053 SET remessa_exp = abap_false
                      WHERE remessa_exp EQ is_likp-vbeln.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_BEFORE_OUTPUT.

*  DATA: wa_lips   TYPE lipsvb,
*        v_task(8) TYPE c,
*        v_rfmng   TYPE vbfa-rfmng.
*
*  DATA: v_id        TYPE  thead-tdid,
*        v_language  TYPE  thead-tdspras,
*        v_name      TYPE  thead-tdname,
*        v_object    TYPE  thead-tdobject,
*        v_exit      TYPE c,
*        wa_zsdt0001 TYPE zsdt0001,
*        wa_lines    TYPE  tline,
*        v_romaneio  TYPE znr_romaneio,
*        wa_vbuk     TYPE vbukvb,
*        sl_vbak     TYPE vbak,
*        sl_vbap     TYPE vbap.
*
*
*  DATA: lt_lines TYPE TABLE OF tline.
*
*  READ TABLE it_xlips INTO wa_lips INDEX 1.
*
*  CLEAR v_exit.
*  READ TABLE it_xvbuk INTO wa_vbuk INDEX 1.
*  IF sy-subrc EQ 0.
*    SELECT SINGLE *
*      FROM vbak
*      INTO sl_vbak
*    WHERE vbeln = wa_vbuk-vbeln.
*  ENDIF.
*
*  "SE FOR PESAGEM OPUS OU SE FOR UMA REMSSA ORIGINADA A PARTIR DE UM PEDIDO DE COMPRA
*  IF sl_vbak-zpesagem EQ '01' OR wa_lips-vgtyp EQ 'V'.
*    "SOMENTE FAZER O PICKING QUANDO O DOCUMENTO FOR GERADO A PARTIR DE UMA ORDEM DE VENDA
*    "E PELA TRANSAÇÃO
*    IF wa_lips-vgtyp EQ 'C' AND  sy-ucomm = 'SICH_T' AND sy-tcode = 'VL01N'.
*      v_task = 'ZPATCH'.
*      v_rfmng = wa_lips-lfimg.
*      "Importa da BADI ZLE_SHP_DELIVERY_PRO método CHANGE_DELIVERY_ITEM
*      "IMPORT wa_zsdt0001 TO wa_zsdt0001 FROM MEMORY ID 'ZSDQUANT'.
*      "DELETE FROM MEMORY ID 'ZSDQUANT'.
*        SELECT SINGLE *
*         FROM zsdt0001
*         INTO wa_zsdt0001
*         WHERE ch_referencia  = wa_lips-kdmat.
*
*      CALL FUNCTION 'Z_SD_UPDATE_PICK' STARTING NEW TASK v_task
*        EXPORTING
*          vbeln      = wa_lips-vbeln
*          pick       = v_rfmng
*          v_zsdt0001 = wa_zsdt0001.
*    ENDIF.
*  ENDIF.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~save_and_publish_document.

    IF sy-tcode EQ 'VL01N' OR
       sy-tcode EQ 'VL02N'.

      DATA: ls_zsdt0406 TYPE zsdt0406,
            lv_mostrar  TYPE char1.

      READ TABLE it_xlikp
          INTO DATA(ls_likp)
          INDEX 1.

      CALL FUNCTION 'ZDELIV_HEAD_SCREEN_VAL_ENABLE'
        EXPORTING
          im_lfart   = ls_likp-lfart
          im_vstel   = ls_likp-vstel
          im_werks   = ls_likp-werks
        IMPORTING
          ex_mostrar = lv_mostrar.

      IF lv_mostrar EQ 'X' AND
         cs_v50agl_cust IS NOT INITIAL.

        CALL FUNCTION 'ZPOST_DELIVERY_HEAD_SCREEN_VAL'
          EXPORTING
            im_likp = ls_likp.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE.

    DATA: SL_LIKP  TYPE LIKPVB,
          VL_TABIX TYPE SYTABIX.

    DATA: WA_SETLEAF TYPE SETLEAF,
          P_DATA_REM TYPE LEDAT,
          ID         TYPE C LENGTH 10 VALUE 'ROMRETRO'.

    SELECT SINGLE *
      FROM SETLEAF
      INTO WA_SETLEAF
     WHERE SETNAME = 'VF01_USUARIO'
       AND VALFROM = SY-UNAME.

    "Programa ZSDI0009 perform MEMORIZAR_DT_MOVIMENTO_BADI
    IF SY-SUBRC IS INITIAL.

      "buscar da memória.
      IMPORT P1 = P_DATA_REM FROM MEMORY ID ID.

      LOOP AT CT_XLIKP INTO SL_LIKP.
        VL_TABIX = SY-TABIX.
        SL_LIKP-BLDAT = P_DATA_REM.
        SL_LIKP-WADAT = P_DATA_REM.
        MODIFY CT_XLIKP FROM SL_LIKP INDEX VL_TABIX TRANSPORTING  WADAT BLDAT.
        CLEAR SL_LIKP.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
