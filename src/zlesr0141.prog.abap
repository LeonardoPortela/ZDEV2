*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Descargas Frota  própria -Serviços Terceiros            *
* Transação..: ZLES0180                                                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zlesr0141.

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: zsds023, zsds024.

*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tela.
         INCLUDE TYPE zlest0194.
TYPES:   nome_motorista TYPE char80.
TYPES: END OF ty_tela.

TYPES: BEGIN OF ty_veicular,
         placa_cav        TYPE zplaca,
         pl_cav_prop_cod  TYPE lifnr,
         pl_cav_prop_cnpj TYPE j_1bcgc,
         pl_cav_tp_veic   TYPE char2,
       END OF ty_veicular.

*----------------------------------------------------------------------*
* Estruturas
*----------------------------------------------------------------------*
DATA: wa_tela     TYPE ty_tela,
      wa_layout   TYPE lvc_s_layo,
      wa_veicular TYPE ty_veicular,
      wa_toolbar  TYPE stb_button,
      wa_contrato TYPE zsds024.

**----------------------------------------------------------------------*
** Variaveis
**----------------------------------------------------------------------*
DATA: v_ucomm             TYPE sy-ucomm,
      v_motorista_cod_sap TYPE lfa1-lifnr,
      v_ctexml            TYPE zlest0194-chave_xml_cte.

DATA: owner    TYPE soud-usrnam,
      sofd_dat LIKE sofdd.


DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

* Gerenciador Arquivos
DATA: manager TYPE REF TO cl_gos_manager,
      obj     TYPE borident,
      ip_mode TYPE sgs_rwmod,
      objtype TYPE borident-objtype VALUE 'ZLES0180'.

DATA: gva_lfa1_stcd3 TYPE lfa1-stcd3,
      gva_kna1_stcd3 TYPE kna1-stcd3.
*---------------------------------------------------------------------
* Classes locais (Definição)
*---------------------------------------------------------------------
*
CLASS lcl_grid_event DEFINITION.
* seção publica
  PUBLIC SECTION.
*...Barra de Ferramentas
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.
*...User Command
    METHODS handle_command_grid
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
ENDCLASS. "LCL_GRID_EVENT DEFINITION

*---------------------------------------------------------------------
* Classes locais (Implementação)
*---------------------------------------------------------------------
CLASS lcl_grid_event IMPLEMENTATION.

  METHOD handle_toolbar.

*...Barra de Ferramentas
    PERFORM f_toolbar_grid CHANGING e_object.

  ENDMETHOD. "handle_toolba

  METHOD handle_command_grid.

*...Rotinas do botão Z da barra de ferramentas do ALV
    v_ucomm = e_ucomm.
    PERFORM f_command USING e_ucomm.

  ENDMETHOD. "handle_command_grid

ENDCLASS. "LCL_GRID_EVENT IMPLEMENTATION
*---------------------------------------------------------------------
*
* Classes ------------------------------------------------------------
*
*---------------------------------------------------------------------
*
DATA: lcl_alv           TYPE REF TO cl_gui_alv_grid,
      lcl_container_alv TYPE REF TO cl_gui_custom_container,
      lcl_event         TYPE REF TO lcl_grid_event.

*----------------------------------------------------------------------*
* Tabelas Internas
*----------------------------------------------------------------------*
DATA: t_zlest0194        TYPE TABLE OF zlest0194,
      t_zsdt0169_seq     TYPE TABLE OF zsdt0244, "zsdt0169_seq,
      t_veicular         TYPE TABLE OF ty_veicular,
      t_contrato         TYPE TABLE OF zsds024,
      t_fieldcat         TYPE lvc_t_fcat,
      t_saida            TYPE TABLE OF zsds022,
      t_zib_cte_dist_ter TYPE TABLE OF zib_cte_dist_ter.

"DATA WL_0400_SEL        TYPE t_saida.

CONTROLS: tbc0200 TYPE TABLEVIEW USING SCREEN 0200.
CONTROLS: tbc0300 TYPE TABLEVIEW USING SCREEN 0200.

*----------------------------------------------------------------------*
* Start-of-Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  "Chamar tela Principal
  CALL SCREEN 0200.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'PF_0200'.
  SET TITLEBAR 'TITULO_0200'.

*----------------------------------------------------------------------*
* Definir campos editáveis
*----------------------------------------------------------------------*
  DATA: l_cpos_fechado TYPE zlest0194-chave_xml_cte,
        l_cpos_aberto  TYPE zib_cte_dist_ter-cd_chave_cte.

  CLEAR: l_cpos_fechado, l_cpos_aberto.

  "Descarga frota própria frete terceiros
  SELECT SINGLE chave_xml_cte
     FROM zlest0194
      INTO l_cpos_fechado
      WHERE chave_xml_cte = wa_tela-chave_xml_cte.

  IF l_cpos_fechado IS INITIAL.

    "Tabela de InBound de CT-e Distribuida
    SELECT SINGLE cd_chave_cte
      FROM zib_cte_dist_ter
      INTO l_cpos_aberto
     WHERE cd_chave_cte = wa_tela-chave_xml_cte.

  ENDIF.

  IF l_cpos_aberto IS NOT INITIAL.

    LOOP AT SCREEN.
      IF screen-group1 = 'GP1'.
        screen-input = 1.

        IF screen-group2 = 'GP2'.
          screen-invisible = '0'.
        ENDIF.

        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSE.

    LOOP AT SCREEN.
      IF screen-group1 = 'GP1'.
        screen-input = 0.

        IF screen-group2 = 'GP2'.
          screen-invisible = '1'.
        ENDIF.

        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      SET SCREEN 0.

    WHEN 'SALVAR'.

      PERFORM f_salvar.

    WHEN 'ELIMINAR'.

      PERFORM f_eliminar.

    WHEN 'ID_CTR'.

      PERFORM f_selecionar_contrato.
*    WHEN 'ANEXO'.
*      PERFORM F_IMPORTAR_ANEXOS.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR
*&---------------------------------------------------------------------*
FORM f_atualizar .

  DATA: vl_branch    TYPE j_1bbranch-branch,
        vl_tarifa    TYPE zde_valor_tarifa,
        vl_encontrou TYPE c.

*&---------------------------------------------------------------------*
*Descarga frota própria frete terceiros
*&---------------------------------------------------------------------*
  SELECT *
    FROM zlest0194
    INTO TABLE t_zlest0194
   WHERE chave_xml_cte = wa_tela-chave_xml_cte.

  IF sy-subrc IS INITIAL.

    READ TABLE t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>) INDEX 1.

    MOVE-CORRESPONDING <fs_zlest0194> TO wa_tela.

**&---------------------------------------------------------------------*
* Buscar textos e descrições
**&---------------------------------------------------------------------*
    PERFORM f_complementa_tela.
*
**&---------------------------------------------------------------------*
*  Busca placas dos veículos
**&---------------------------------------------------------------------*
    PERFORM f_preenche_veicular.
*
**&---------------------------------------------------------------------*
*   Busca contratos vinculados
**&---------------------------------------------------------------------*
    PERFORM f_preenche_contrato.

  ELSE.

    SELECT *
      FROM zib_cte_dist_ter
      INTO TABLE t_zib_cte_dist_ter
     WHERE cd_chave_cte = wa_tela-chave_xml_cte.

    IF sy-subrc IS INITIAL.

      READ TABLE t_zib_cte_dist_ter ASSIGNING FIELD-SYMBOL(<fs_zib_cte_dist_ter>) INDEX 1.

      MOVE-CORRESPONDING <fs_zib_cte_dist_ter> TO wa_tela.
      wa_tela-meins = 'KG'.
      wa_tela-chave_xml_cte = <fs_zib_cte_dist_ter>-cd_chave_cte.

*** - US - 82085 - Inicio - CBRAND ( Comentamos apos novos ajustes da US )
*      DATA(_set_reme_exped) = abap_false.
*      IF ( wa_tela-reme_cnpj IS INITIAL  AND wa_tela-reme_cnpj IS INITIAL ) AND  "Parceiro Exterior
*         ( wa_tela-exped_cnpj IS NOT INITIAL OR wa_tela-exped_cpf IS NOT INITIAL  ). "Assumir Expedidor da operação
*        _set_reme_exped = abap_true.
*      ELSEIF ( wa_tela-reme_cnpj IS NOT INITIAL  ) AND ( wa_tela-reme_cnpj <> wa_tela-exped_cnpj ).
*        _set_reme_exped = abap_true.
*      ELSEIF ( wa_tela-reme_cpf IS NOT INITIAL ) AND ( wa_tela-reme_cpf <> wa_tela-exped_cpf ).
*        _set_reme_exped = abap_true.
*      ENDIF.
*
*      IF _set_reme_exped EQ abap_true.
*        wa_tela-reme_cnpj         =  wa_tela-exped_cnpj.
*        wa_tela-reme_cpf          =  wa_tela-exped_cpf.
*        wa_tela-reme_ie           =  wa_tela-exped_ie.
*        wa_tela-reme_rsocial      =  wa_tela-exped_rsocial.
*        wa_tela-reme_cod_forn     =  wa_tela-exped_cod_forn.
*      ENDIF.
*** - US - 82085 - Fim - CBRAND ( Comentamos apos novos ajustes da US )

**&---------------------------------------------------------------------*
* Buscar textos e descrições
**&---------------------------------------------------------------------*
      PERFORM f_complementa_tela.

    ENDIF.

  ENDIF.

  "*MSG:  Xml não localizado, por favor solicitar a transportadora o envio!
  IF t_zlest0194[]        IS INITIAL AND t_zib_cte_dist_ter[] IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-004 TEXT-005 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SALVAR
*&---------------------------------------------------------------------*
FORM f_salvar .

  DATA: vl_save TYPE c.

  vl_save = abap_true.

  IF wa_tela-chave_xml_cte  IS INITIAL .
    CLEAR vl_save .
    MESSAGE s000(z_les) WITH 'Informe a Chave XML CTe!'
     DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF  wa_tela-qt_descarga_cte IS INITIAL.
    CLEAR vl_save .
    MESSAGE s000(z_les) WITH  'Informe a Peso Descarga do CTe!'
       DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF wa_tela-meins            IS INITIAL.
    CLEAR vl_save .
    MESSAGE s000(z_les) WITH  'Informe a unidade de medida!'
     DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF wa_tela-dt_descarga     IS INITIAL.
    CLEAR vl_save .
    MESSAGE s000(z_les) WITH  'Informe a Data de Descarga do CTe!'
        DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF wa_tela-motorista       IS INITIAL.
    CLEAR vl_save .
    MESSAGE s000(z_les) WITH  'Informe os dados do Motorista!'
       DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF  wa_tela-frota            IS INITIAL.
    CLEAR vl_save .
    MESSAGE s000(z_les) WITH  'Informe os dados da Frota!'
     DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF t_veicular[] IS INITIAL.
    CLEAR vl_save .
    MESSAGE s000(z_les) WITH 'Nenhum conjunto veicular encontrado' 'para a frota informada!'
    DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF t_contrato[] IS INITIAL.
    CLEAR vl_save .
    MESSAGE s000(z_les) WITH 'Nenhum Contrato atribuido ao CTe!'
    DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF t_zlest0194[] IS NOT INITIAL.
    READ TABLE t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>) WITH KEY chave_xml_cte = wa_tela-chave_xml_cte.
    IF sy-subrc IS INITIAL.
      IF <fs_zlest0194>-ov_sub IS NOT INITIAL.
        CLEAR vl_save .
        MESSAGE s000(z_les) WITH TEXT-007 <fs_zlest0194>-ov_sub '!' DISPLAY LIKE 'S'.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  IF vl_save = abap_true.
    CLEAR vl_save .

    LOOP AT t_contrato ASSIGNING FIELD-SYMBOL(<fs_contrato>).

      CHECK <fs_contrato>-id_ctr IS NOT INITIAL.

      wa_tela-id_ctr     = <fs_contrato>-id_ctr.

      SELECT SINGLE * FROM zsdt0244
        INTO @DATA(w_zsdt0244)
        WHERE id_ctr = @<fs_contrato>-id_ctr
          AND ano    = @<fs_contrato>-ano.

      wa_tela-ano     = w_zsdt0244-ano.

      "Atualiza quantidade de embarque do contrato
      w_zsdt0244-total_embarque = w_zsdt0244-total_embarque + wa_tela-qt_carga_cte.

      "Verifica tolerância para bloqueio do contrato
      DATA(l_tol_min) = ( w_zsdt0244-quantidade - ( w_zsdt0244-quantidade * w_zsdt0244-tolerancia ) / 100 ).

      IF l_tol_min <= w_zsdt0244-total_embarque.
        w_zsdt0244-status = '0002'.
      ENDIF.

      MODIFY zsdt0244 FROM w_zsdt0244.
      COMMIT WORK.

    ENDLOOP.

    SORT t_veicular BY pl_cav_tp_veic ASCENDING.
    LOOP AT t_veicular ASSIGNING FIELD-SYMBOL(<fs_veicular>).
      CASE sy-tabix.
        WHEN '1'.
          wa_tela-placa_cav        = <fs_veicular>-placa_cav.
          wa_tela-pl_cav_prop_cod  = <fs_veicular>-pl_cav_prop_cod.
          wa_tela-pl_cav_prop_cnpj = <fs_veicular>-pl_cav_prop_cnpj.
          wa_tela-pl_cav_tp_veic   = <fs_veicular>-pl_cav_tp_veic.

        WHEN '2'.
          wa_tela-placa_car1        = <fs_veicular>-placa_cav.
          wa_tela-pl_car1_prop_cod  = <fs_veicular>-pl_cav_prop_cod.
          wa_tela-pl_car1_prop_cnpj = <fs_veicular>-pl_cav_prop_cnpj.
          wa_tela-pl_car1_tp_veic   = <fs_veicular>-pl_cav_tp_veic.

        WHEN '3'.
          wa_tela-placa_car2        = <fs_veicular>-placa_cav.
          wa_tela-pl_car2_prop_cod  = <fs_veicular>-pl_cav_prop_cod.
          wa_tela-pl_car2_prop_cnpj = <fs_veicular>-pl_cav_prop_cnpj.
          wa_tela-pl_car2_tp_veic   = <fs_veicular>-pl_cav_tp_veic.

        WHEN '4'.
          wa_tela-placa_car3        = <fs_veicular>-placa_cav.
          wa_tela-pl_car3_prop_cod  = <fs_veicular>-pl_cav_prop_cod.
          wa_tela-pl_car3_prop_cnpj = <fs_veicular>-pl_cav_prop_cnpj.
          wa_tela-pl_car3_tp_veic   = <fs_veicular>-pl_cav_tp_veic.

      ENDCASE.

      "Atualiza o valor da tarifa com o valor atual do contrato
      wa_tela-valor_prestacao = ( wa_tela-qt_carga_cte / 1000 ) * w_zsdt0244-tarifa.

      MODIFY zlest0194 FROM wa_tela.
      COMMIT WORK.
      vl_save = abap_true.
    ENDLOOP.

    MESSAGE s000(z_les) WITH TEXT-012 DISPLAY LIKE 'S'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ELIMINAR
*&---------------------------------------------------------------------*
FORM f_eliminar .

  DATA:  l_answer     TYPE c.

* Verifica o registro na tabela Z
  IF wa_tela-chave_xml_cte IS INITIAL.
    MESSAGE s000(z_les) WITH 'Chave XML de CTe Terceiros não informada!'
                  DISPLAY LIKE 'E'.
  ELSE.


    SELECT * FROM zlest0194
      INTO TABLE @DATA(t_delete)
      WHERE chave_xml_cte = @wa_tela-chave_xml_cte.
**********************************************************************
* TRATAMENTO ov_sub - PSA
**********************************************************************
    DATA lr_ov_sub TYPE RANGE OF zlest0194-ov_sub.

    lr_ov_sub[] = VALUE #( FOR wa_params2 IN t_delete WHERE ( ov_sub <> space ) ( sign = 'I' option = 'EQ' low = wa_params2-ov_sub ) ).
    SORT: lr_ov_sub BY low.
    DELETE ADJACENT DUPLICATES FROM lr_ov_sub COMPARING ALL FIELDS.
**********************************************************************

    IF lr_ov_sub IS INITIAL . "PSA













      IF t_delete[] IS NOT INITIAL.


        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'(020)
            text_question         = 'Deseja realmente eliminar o registro ?'(021)
            text_button_1         = 'Sim'(022)
            text_button_2         = 'Não'(023)
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = l_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF l_answer = 1.

        ELSE.

          MESSAGE s000(z_les) WITH 'Chave: ZLEST0194-CHAVE_XML_CTE com '
                              'OV gerada - ZLEST0194-OV_SUB.'
                               'Necessário estornar faturamento Frete Subcontratação!' DISPLAY LIKE 'E'.

        ENDIF.

        DELETE zlest0194 FROM TABLE t_delete.
        COMMIT WORK.

        LOOP AT t_delete INTO DATA(w_delete).

          "Busca contrato para subtrair o peso do total embarcado
          SELECT SINGLE * FROM zsdt0244
            INTO @DATA(w_zsdt0244)
            WHERE id_ctr = @w_delete-id_ctr.

          IF sy-subrc IS INITIAL.

            w_zsdt0244-total_embarque =  ( w_zsdt0244-total_embarque - w_delete-qt_carga_cte ).

            IF w_zsdt0244-status = '0002'.

              DATA(l_tol_min) = ( w_zsdt0244-quantidade - ( w_zsdt0244-quantidade * w_zsdt0244-tolerancia ) / 100 ).
              IF l_tol_min > w_zsdt0244-total_embarque.
                w_zsdt0244-status = '0001'.
              ENDIF.

            ENDIF.

          ENDIF.

          MODIFY zsdt0244 FROM w_zsdt0244.
          COMMIT WORK.

        ENDLOOP.

        IF sy-subrc IS INITIAL.
          CLEAR: t_veicular, t_contrato, wa_veicular, wa_contrato, wa_tela.
          MESSAGE s000(z_les) WITH 'Registro eliminado da tabela'
                                'de descarga frota própria'
                                 'frete terceiros !'.
        ENDIF.

      ENDIF.

    ELSE.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Não é possivel Estornar!'
          txt1  = 'Chave: ' && wa_tela-chave_xml_cte && ' com '
          txt2  = 'OV gerada - ' && wa_tela-ov_sub && '.'
          txt3  = 'Necessário estornar faturamento Frete Subcontratação!'.


    ENDIF.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COMPLEMENTA_TELA
*&---------------------------------------------------------------------*
FORM f_complementa_tela .

* Códigos  de cliente e  fornecedor  para  emitente
  IF wa_tela-emit_cnpj IS NOT INITIAL.
    PERFORM f_busca_kunnr_emit USING 'CNPJ'
                            CHANGING wa_tela-emit_cod_cliente.
  ELSEIF wa_tela-emit_cpf IS NOT INITIAL.
    PERFORM f_busca_kunnr_emit USING 'CPF'
                            CHANGING wa_tela-emit_cod_cliente.
  ENDIF.


  IF wa_tela-emit_cnpj IS NOT INITIAL.
    PERFORM f_busca_lifnr_emit USING 'CNPJ'
                            CHANGING wa_tela-emit_cod_forn.
  ELSEIF wa_tela-emit_cpf IS NOT INITIAL.
    PERFORM f_busca_lifnr_emit USING 'CPF'
                            CHANGING wa_tela-emit_cod_forn.
  ENDIF.


* Códigos  de  fornecedor  para  remetente
  IF wa_tela-reme_cnpj IS NOT INITIAL.
    PERFORM f_busca_lifnr_remet USING 'CNPJ'
                           CHANGING wa_tela-reme_cod_forn.
  ELSEIF wa_tela-reme_cpf IS NOT INITIAL.
    PERFORM f_busca_lifnr_remet USING 'CPF'
                            CHANGING wa_tela-reme_cod_forn.
  ENDIF.


* Códigos  de  fornecedor  para  recebedor
  IF wa_tela-exped_cnpj  IS NOT INITIAL.
    PERFORM f_busca_lifnr_receb USING 'CNPJ'
                        CHANGING wa_tela-exped_cod_forn.
  ELSEIF wa_tela-exped_cpf IS NOT INITIAL.
    PERFORM f_busca_lifnr_receb USING 'CPF'
                         CHANGING wa_tela-exped_cod_forn.
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
  ELSE.
    PERFORM f_busca_lifnr_receb USING ''
                         CHANGING wa_tela-exped_cod_forn.
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Fim de Alteração
  ENDIF.

*****  US - 82085 - Inicio  - CBRAND
***** Códigos  de cliente para  destinatário
  IF wa_tela-dest_cnpj IS NOT INITIAL.
    PERFORM f_busca_kunnr_dest USING 'CNPJ'
                            CHANGING wa_tela-dest_cod_cliente.
  ELSEIF wa_tela-dest_cpf IS NOT INITIAL.
    PERFORM f_busca_kunnr_dest USING 'CPF'
                            CHANGING wa_tela-dest_cod_cliente.
  ENDIF.
*****  US - 82085 - Fim  - CBRAND

*****  US - 82085 - Inicio  - CBRAND
  "73107
*** Códigos  de cliente para  destinatário
*  IF wa_tela-receb_cnpj IS NOT INITIAL.
*    PERFORM f_busca_kunnr_dest USING 'CNPJ'
*                            CHANGING wa_tela-dest_cod_cliente.
*  ELSEIF wa_tela-receb_cpf IS NOT INITIAL.
*    PERFORM f_busca_kunnr_dest USING 'CPF'
*                            CHANGING wa_tela-dest_cod_cliente.
*  ENDIF.
*****  US - 82085 - Fim  - CBRAND
*  IF wa_tela-dest_cnpj IS NOT INITIAL AND  wa_tela-receb_cpf IS INITIAL AND wa_tela-receb_cnpj IS INITIAL.
*    PERFORM f_busca_kunnr_dest USING 'CNPJ'
*                            CHANGING wa_tela-dest_cod_cliente.
*  ENDIF.
*<<" end 73107

* Códigos  de cliente para  recebedor
  IF wa_tela-receb_cnpj  IS NOT INITIAL.
    PERFORM f_busca_kunnr_receb USING 'CNPJ'
                        CHANGING wa_tela-receb_cod_cliente.
  ELSEIF wa_tela-receb_cpf IS NOT INITIAL.
    PERFORM f_busca_kunnr_receb USING 'CPF'
                         CHANGING wa_tela-receb_cod_cliente.
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
  ELSE.
    PERFORM f_busca_kunnr_receb USING ''
                         CHANGING wa_tela-receb_cod_cliente.
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Fim de Alteração
  ENDIF.

  IF wa_tela-mot_cpf IS NOT INITIAL.
    PERFORM f_buscar_motorista.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PREENCHE_SCREEN_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE preenche_screen_0200 OUTPUT.

  MOVE-CORRESPONDING wa_veicular TO zsds023.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PREENCHE_SCREEN_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE preenche_screen_0300 OUTPUT.

  MOVE-CORRESPONDING wa_contrato TO zsds024.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PREENCHE_PLACA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE preenche_placa INPUT.

  DATA: vl_erro TYPE c.

* Seleção para  conjunto veicular
  SELECT SINGLE *
    FROM zlest0002
   WHERE pc_veiculo = @zsds023-placa_cav
    INTO @DATA(wa_zlest0002).

  IF sy-subrc IS INITIAL.

** Só pode inserir uma categoria por placa
    IF t_veicular[] IS NOT INITIAL.

      LOOP AT t_veicular ASSIGNING FIELD-SYMBOL(<fs_veicular_2>).
        IF <fs_veicular_2>-pl_cav_tp_veic = 1 AND
           wa_zlest0002-ct_veiculo        = 1.
          vl_erro = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF vl_erro = abap_true.
        MESSAGE s000(z_les) WITH TEXT-008 DISPLAY LIKE 'S'.
        RETURN.
      ENDIF.

    ENDIF.

    IF wa_zlest0002-proprietario IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1
       WHERE land1 = 'BR'
         AND lifnr = @wa_zlest0002-proprietario
        INTO @DATA(wa_lfa1).
    ENDIF.

    wa_veicular-placa_cav        = zsds023-placa_cav.
    wa_veicular-pl_cav_prop_cod  = wa_zlest0002-proprietario.

    IF wa_lfa1-stcd1 IS NOT INITIAL.
      wa_veicular-pl_cav_prop_cnpj = wa_lfa1-stcd1.
    ELSEIF wa_lfa1-stcd2 IS NOT INITIAL.
      wa_veicular-pl_cav_prop_cnpj = wa_lfa1-stcd2.
    ENDIF.

    wa_veicular-pl_cav_tp_veic   = wa_zlest0002-ct_veiculo.
    APPEND wa_veicular TO t_veicular.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LIMPA_TABELAS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE limpa_tabelas INPUT.

  CLEAR: t_veicular, t_contrato, wa_veicular, wa_contrato.

  IF  wa_tela-chave_xml_cte <> v_ctexml.
    v_ctexml = wa_tela-chave_xml_cte.
    CLEAR wa_tela.
    wa_tela-chave_xml_cte =  v_ctexml.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PREENCHE_CONTRATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE preenche_contrato INPUT.

  DATA: vl_lines       LIKE sy-index,
        vl_unid_tarifa TYPE zde_qt_carga_cte.

  IF wa_tela-emit_cod_cliente IS NOT INITIAL.

*    vl_unid_tarifa = ( wa_tela-valor_prestacao / wa_tela-qt_carga_cte ) * 1000.

    CLEAR: t_zsdt0169_seq, t_saida.
    SELECT *
      FROM zsdt0244 "zsdt0169_seq
      INTO TABLE t_zsdt0169_seq
     WHERE id_cliente   = wa_tela-emit_cod_cliente
      "AND  pc_codigo    = wa_tela-reme_cod_forn "US - 82085 - CBRAND
      AND  pc_codigo = wa_tela-exped_cod_forn "US - 82085 - CBRAND
      AND  lr_codigo = wa_tela-dest_cod_cliente
     " AND
      AND status        = '0001'.

    LOOP AT t_zsdt0169_seq ASSIGNING FIELD-SYMBOL(<fs_zsdt0169_seq>).

      wa_contrato-id_ctr       = <fs_zsdt0169_seq>-id_ctr.
      wa_contrato-ano          = <fs_zsdt0169_seq>-ano.
      wa_contrato-tarifa       = <fs_zsdt0169_seq>-tarifa.
      wa_contrato-unid_tarifa  = <fs_zsdt0169_seq>-unid_tarifa.
      APPEND wa_contrato TO t_contrato.
      CLEAR: wa_contrato.

    ENDLOOP.

    IF t_zsdt0169_seq[] IS NOT INITIAL.
      MOVE-CORRESPONDING t_zsdt0169_seq TO t_saida.
      CALL SCREEN 0400.
    ENDIF.

  ENDIF.

ENDMODULE.

FORM f_command USING p_ucomm TYPE sy-ucomm.

  CASE v_ucomm.

    WHEN 'SELECIONAR'.

  ENDCASE.

ENDFORM. " F_command

FORM f_toolbar_grid CHANGING p_object TYPE REF TO cl_alv_event_toolbar_set.


  CLEAR wa_toolbar.
  MOVE: 'SELECIONAR' TO wa_toolbar-function,
  icon_checked TO wa_toolbar-icon,
  TEXT-002 TO wa_toolbar-text,
  space TO wa_toolbar-disabled.
  APPEND wa_toolbar TO p_object->mt_toolbar.

  DELETE p_object->mt_toolbar WHERE function <> 'SELECIONAR'
                                AND function <> '&MB_EXPORT'
                                AND function <> '&SORT_ASC'
                                AND function <> '&SORT_DSC'
                                AND function <> '&PRINT_BACK'.

ENDFORM. " F_toolbar_grid
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.

  SET PF-STATUS 'PF_0400'.
  SET TITLEBAR 'TITULO_0400'.

  PERFORM f_exibe_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      SET SCREEN 0.

    WHEN 'SELECIONAR'.

      PERFORM f_valida_contrato_selecionado.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_CONTRATO_SELECIONADO
*&---------------------------------------------------------------------*
FORM f_valida_contrato_selecionado.

  DATA: vl_valida TYPE char200.

  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
    IF <fs_saida>-selecionar = abap_true.
      vl_valida = vl_valida + 1.
    ENDIF.
  ENDLOOP.

  IF vl_valida NE 1.

    MESSAGE s000(z_les) WITH TEXT-003 DISPLAY LIKE 'S'.

  ELSEIF vl_valida = 1.

    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_zsdt0169_seq>) WITH KEY selecionar = abap_true.
    wa_contrato-id_ctr       = <fs_zsdt0169_seq>-id_ctr.
    wa_contrato-tarifa       = <fs_zsdt0169_seq>-tarifa.
    wa_contrato-unid_tarifa  = <fs_zsdt0169_seq>-unid_tarifa.
    APPEND wa_contrato TO t_contrato.
    CLEAR: wa_contrato.
    RETURN.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  IF lcl_alv IS BOUND.
    CALL METHOD lcl_alv->free.
    CLEAR lcl_alv.
  ENDIF.

  IF lcl_container_alv IS BOUND.
    CALL METHOD lcl_container_alv->free.
    CLEAR lcl_container_alv.
  ENDIF.

  CREATE OBJECT lcl_alv
    EXPORTING
      i_parent          = cl_gui_container=>screen0 "LCL_CONTAINER_ALV
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  CREATE OBJECT lcl_event.

* Incluir a referência a o evento TOOLBAR
  SET HANDLER lcl_event->handle_toolbar FOR lcl_alv.

* Incluir a referência a o evento USER_COMMAND
  SET HANDLER lcl_event->handle_command_grid FOR lcl_alv.

  wa_layout-zebra = abap_true.
  wa_layout-cwidth_opt = abap_true.

  REFRESH t_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSDS022'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  READ TABLE t_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>) INDEX 1.
  <fs_fieldcat>-checkbox = abap_true.
  <fs_fieldcat>-edit = abap_true.
  <fs_fieldcat>-outputlen = 2.

  CALL METHOD lcl_alv->set_table_for_first_display
    EXPORTING
      i_save          = 'A'
      i_default       = 'X'
      is_layout       = wa_layout
    CHANGING
      it_outtab       = t_saida[]
      it_fieldcatalog = t_fieldcat[].


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_KUNNR_EMIT
*&---------------------------------------------------------------------*
FORM f_busca_kunnr_emit USING p_codigo TYPE char4
                     CHANGING p_retorno TYPE kna1-kunnr.


  CLEAR: gva_kna1_stcd3.

  IF p_codigo = 'CNPJ'.

*** BUG - 89608 - Inicio -  CBRAND
*        SELECT *
*      FROM kna1
*     WHERE land1 = 'BR'
*       AND stcd1 = @wa_tela-emit_cnpj
*       AND stcd3 = @wa_tela-emit_ie
*     INTO @DATA(vl_kunnr_1).
*
*    p_retorno = vl_kunnr_1.

    SELECT *
      FROM kna1
     WHERE land1 = 'BR'
       AND stcd1 = @wa_tela-emit_cnpj
      INTO TABLE @DATA(t_kna1).


    LOOP AT t_kna1 INTO DATA(lwa_kna1).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_kna1-stcd3
        IMPORTING
          output = lwa_kna1-stcd3.

      CLEAR gva_kna1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-emit_ie
        IMPORTING
          output = gva_kna1_stcd3.

      IF lwa_kna1-stcd3 = gva_kna1_stcd3.
        p_retorno = lwa_kna1-kunnr.
      ENDIF.

      CLEAR: lwa_kna1.
    ENDLOOP.
*** BUG - 89608 - Fim - CBRAND


  ELSEIF p_codigo = 'CPF'.

*** BUG - 89608 - Inicio -  CBRAND

*    SELECT SINGLE kunnr
*      FROM kna1
*     WHERE land1 = 'BR'
*       AND stcd2 = @wa_tela-emit_cpf
*       AND stcd3 = @wa_tela-emit_ie
*      INTO @DATA(vl_kunnr_2).
*
*    p_retorno = vl_kunnr_2.


    SELECT *
      FROM kna1
     WHERE land1 = 'BR'
       AND stcd2 = @wa_tela-emit_cpf
      INTO TABLE @DATA(t_kna2).

    LOOP AT t_kna2 INTO DATA(lwa_kna2).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_kna2-stcd3
        IMPORTING
          output = lwa_kna2-stcd3.

      CLEAR gva_kna1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-emit_ie
        IMPORTING
          output = gva_kna1_stcd3.

      IF lwa_kna1-stcd3 = gva_kna1_stcd3.
        p_retorno = lwa_kna2-kunnr.
      ENDIF.

      CLEAR: lwa_kna1.
    ENDLOOP.
*** BUG - 89608 - Fim - CBRAND

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_LIFNR_EMIT
*&---------------------------------------------------------------------*
FORM f_busca_lifnr_emit  USING p_codigo  TYPE char4
                      CHANGING p_retorno TYPE lfa1-lifnr.


  IF p_codigo = 'CNPJ'.

*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE lifnr
*      FROM lfa1
*     WHERE land1 = 'BR'
*       AND stcd1 = @wa_tela-emit_cnpj
*       AND stcd3 = @wa_tela-emit_ie
*      INTO @DATA(vl_lifnr_1).
*
*    p_retorno = vl_lifnr_1.

    SELECT *
      FROM lfa1
      WHERE land1 = 'BR'
      AND stcd1 = @wa_tela-emit_cnpj
      INTO TABLE @DATA(t_lfa1).

    LOOP AT t_lfa1 INTO DATA(lwa_lfa1).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_lfa1-stcd3
        IMPORTING
          output = lwa_lfa1-stcd3.

      CLEAR gva_lfa1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-emit_ie
        IMPORTING
          output = gva_lfa1_stcd3.

      IF lwa_lfa1-stcd3 = gva_lfa1_stcd3.
        p_retorno = lwa_lfa1-lifnr.
      ENDIF.

      CLEAR: lwa_lfa1.
    ENDLOOP.
*** BUG - 89608 - Fim - CBRAND


  ELSEIF p_codigo = 'CPF'.

*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE lifnr
*     FROM lfa1
*    WHERE land1 = 'BR'
*      AND stcd2 = @wa_tela-emit_cpf
*      AND stcd3 = @wa_tela-emit_ie
*     INTO @DATA(vl_lifnr_2).
*
*    p_retorno = vl_lifnr_2.

    CLEAR: t_lfa1, lwa_lfa1.

    SELECT *
     FROM lfa1
      INTO TABLE t_lfa1
    WHERE land1 = 'BR'
      AND stcd2 = wa_tela-emit_cpf.

    LOOP AT t_lfa1 INTO lwa_lfa1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_lfa1-stcd3
        IMPORTING
          output = lwa_lfa1-stcd3.

      CLEAR gva_lfa1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-emit_ie
        IMPORTING
          output = gva_lfa1_stcd3.

      IF lwa_lfa1-stcd3 = gva_lfa1_stcd3.
        p_retorno = lwa_lfa1-lifnr.
      ENDIF.
      CLEAR: lwa_lfa1.
    ENDLOOP.
*** BUG - 89608 - Fim - CBRAND
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_LIFNR_REMET
*&---------------------------------------------------------------------*
FORM f_busca_lifnr_remet  USING p_codigo TYPE char4
                       CHANGING p_retorno TYPE lfa1-lifnr.


  IF p_codigo = 'CNPJ'.
*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE lifnr
*      FROM lfa1
*     WHERE land1 = 'BR'
*       AND stcd1 = @wa_tela-reme_cnpj
*       AND stcd3 = @wa_tela-reme_ie
*      INTO @DATA(vl_lifnr_1).
*
*    p_retorno = vl_lifnr_1.

    SELECT *
      FROM lfa1
      WHERE land1 = 'BR'
      AND stcd1 = @wa_tela-reme_cnpj
      INTO TABLE @DATA(t_lfa1).

    LOOP AT t_lfa1 INTO DATA(lwa_lfa1).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_lfa1-stcd3
        IMPORTING
          output = lwa_lfa1-stcd3.

      CLEAR gva_lfa1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-reme_ie
        IMPORTING
          output = gva_lfa1_stcd3.

      IF lwa_lfa1-stcd3 = gva_lfa1_stcd3.
        p_retorno = lwa_lfa1-lifnr.
      ENDIF.

      CLEAR: lwa_lfa1.
    ENDLOOP.

  ELSEIF p_codigo = 'CPF'.

*    SELECT SINGLE lifnr
*      FROM lfa1
*     WHERE land1 = 'BR'
*       AND stcd2 = @wa_tela-reme_cpf
*       AND stcd3 = @wa_tela-reme_ie
*      INTO @DATA(vl_lifnr_2).
*
*    p_retorno = vl_lifnr_2.

    CLEAR: t_lfa1, lwa_lfa1.

    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1
      WHERE land1 = 'BR'
      AND stcd2 = wa_tela-reme_cpf.


    LOOP AT t_lfa1 INTO lwa_lfa1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_lfa1-stcd3
        IMPORTING
          output = lwa_lfa1-stcd3.

      CLEAR gva_lfa1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-reme_ie
        IMPORTING
          output = gva_lfa1_stcd3.

      IF lwa_lfa1-stcd3 = gva_lfa1_stcd3.
        p_retorno = lwa_lfa1-lifnr.
      ENDIF.

      CLEAR: lwa_lfa1.
    ENDLOOP.
*** BUG - 89608 - Fim - CBRAND
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_KUNNR_DEST
*&---------------------------------------------------------------------*
FORM f_busca_kunnr_dest  USING p_codigo  TYPE char4
                      CHANGING p_retorno TYPE kna1-kunnr.

  IF p_codigo = 'CNPJ'.
*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE kunnr
*        FROM kna1
*       WHERE land1 = 'BR'
*       "  AND stcd1 = @wa_tela-dest_cnpj US: 73107
*         AND stcd1 = @wa_tela-receb_cnpj
*         AND stcd3 = @wa_tela-receb_ie
*        INTO @DATA(vl_kunnr_1).
*
*    "p_retorno = vl_kunnr_1.
*    IF sy-subrc NE 0.
*
*      SELECT SINGLE kunnr
*        FROM kna1
*        INTO @vl_kunnr_1
*        WHERE land1 = 'BR'
*         AND stcd1 = @wa_tela-dest_cnpj "
*         "AND stcd1 = @wa_tela-receb_cnpj US: 73107
*         AND stcd3 = @wa_tela-dest_ie.
*      "INTO @DATA(vl_kunnr_1).
*
*      wa_tela-receb_rsocial = wa_tela-dest_rsocial.
*      wa_tela-receb_cnpj    = wa_tela-dest_cnpj.
*      wa_tela-receb_ie      = wa_tela-dest_ie.
*
*      " p_retorno = vl_kunnr_1.
*    ENDIF.
*
*    p_retorno = vl_kunnr_1.

*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
*    SELECT *
*      FROM kna1
*     WHERE land1 = 'BR'
*       AND stcd1 = @wa_tela-receb_cnpj
*      INTO TABLE @DATA(t_kna2).

*    IF sy-subrc EQ 0.
*
*      LOOP AT t_kna2 INTO DATA(lwa_kna2).
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = lwa_kna2-stcd3
*          IMPORTING
*            output = lwa_kna2-stcd3.
*
*        CLEAR gva_kna1_stcd3.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = wa_tela-receb_ie
*          IMPORTING
*            output = gva_kna1_stcd3.
*
*        IF lwa_kna2-stcd3 = gva_kna1_stcd3.
*          p_retorno = lwa_kna2-kunnr.
*        ENDIF.
*
*        CLEAR: lwa_kna2.
*      ENDLOOP.
*    ELSE.

*    CLEAR: t_kna2, lwa_kna2.
*
*    SELECT *
*      FROM kna1
*      INTO TABLE t_kna2
*      WHERE land1 = 'BR'
*       AND stcd1 = wa_tela-dest_cnpj.

    SELECT *
      FROM kna1
      INTO TABLE @DATA(t_kna2)
      WHERE land1 = 'BR'
       AND stcd1 = @wa_tela-dest_cnpj.
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Fim de Alteração

*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
*    wa_tela-receb_rsocial = wa_tela-dest_rsocial.
*    wa_tela-receb_cnpj    = wa_tela-dest_cnpj.
*    wa_tela-receb_ie      = wa_tela-dest_ie.

*    LOOP AT t_kna2 INTO lwa_kna2.
    LOOP AT t_kna2 INTO DATA(lwa_kna2).
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Fim de Alteração

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_kna2-stcd3
        IMPORTING
          output = lwa_kna2-stcd3.

      CLEAR gva_kna1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-dest_ie
        IMPORTING
          output = gva_kna1_stcd3.

      IF lwa_kna2-stcd3 = gva_kna1_stcd3.
        p_retorno = lwa_kna2-kunnr.
      ENDIF.

      CLEAR: lwa_kna2.
    ENDLOOP.

*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
*    ENDIF.
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Fim de Alteração
*** BUG - 89608 - Fim - CBRAND



  ELSEIF p_codigo = 'CPF'.

*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE kunnr
*        FROM kna1
*       WHERE land1 = 'BR'
*        " AND stcd2 = @wa_tela-dest_cpf US: 73107
*         AND stcd2 = @wa_tela-receb_cpf
*         AND stcd3 = @wa_tela-receb_ie
*        INTO @DATA(vl_kunnr_2).
*
*    " p_retorno = vl_kunnr_2.
*
*    IF sy-subrc NE 0.
*                                                            "US: 73107
*      SELECT SINGLE kunnr
*       FROM kna1
*      WHERE land1 = 'BR'
*       AND stcd2 = @wa_tela-dest_cpf
*       AND stcd3 = @wa_tela-dest_ie
*       INTO @vl_kunnr_2.
*
*      wa_tela-receb_rsocial = wa_tela-dest_rsocial.
*      wa_tela-receb_cpf = wa_tela-dest_cpf.
*      wa_tela-receb_ie = wa_tela-dest_ie.
*
*    ENDIF.                                                  "US: 73107
*    p_retorno = vl_kunnr_2.

    CLEAR: t_kna2, lwa_kna2.

    SELECT *
      FROM kna1
      INTO TABLE t_kna2
     WHERE land1 = 'BR'
       AND stcd2 = wa_tela-receb_cpf
       AND loevm <> abap_true. "FF #178888

    IF sy-subrc EQ 0.

      LOOP AT t_kna2 INTO lwa_kna2.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = lwa_kna2-stcd3
          IMPORTING
            output = lwa_kna2-stcd3.

        CLEAR gva_kna1_stcd3.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_tela-receb_ie
          IMPORTING
            output = gva_kna1_stcd3.

        IF lwa_kna2-stcd3 = gva_kna1_stcd3.
          p_retorno = lwa_kna2-kunnr.
        ENDIF.

        CLEAR: lwa_kna2.
      ENDLOOP.
    ELSE.

      CLEAR: t_kna2, lwa_kna2.

      SELECT *
        FROM kna1
        INTO TABLE t_kna2
        WHERE land1 = 'BR'
         AND stcd2 = wa_tela-dest_cpf
         AND loevm <> abap_true. "FF #178888.

      wa_tela-receb_rsocial = wa_tela-dest_rsocial.
      wa_tela-receb_cpf     = wa_tela-dest_cpf.
      wa_tela-receb_ie      = wa_tela-dest_ie.

      LOOP AT t_kna2 INTO lwa_kna2.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = lwa_kna2-stcd3
          IMPORTING
            output = lwa_kna2-stcd3.

        CLEAR gva_kna1_stcd3.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_tela-dest_ie
          IMPORTING
            output = gva_kna1_stcd3.

        IF lwa_kna2-stcd3 = gva_kna1_stcd3.
          p_retorno = lwa_kna2-kunnr.
        ENDIF.

        CLEAR: lwa_kna2.
      ENDLOOP.

    ENDIF.
*** BUG - 89608 - Fim - CBRAND
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_VEICULAR
*&---------------------------------------------------------------------*
FORM f_preenche_veicular .

  DATA: vl_contador TYPE c.

  CLEAR: t_veicular.
  LOOP AT t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>).

    DO 4 TIMES.
      vl_contador = vl_contador + 1.

      CASE vl_contador.
        WHEN '1'.
          IF <fs_zlest0194>-placa_cav IS NOT INITIAL.

            APPEND INITIAL LINE TO t_veicular  ASSIGNING FIELD-SYMBOL(<fs_veicular>).
            <fs_veicular>-placa_cav        = <fs_zlest0194>-placa_cav.
            <fs_veicular>-pl_cav_prop_cod  = <fs_zlest0194>-pl_cav_prop_cod.
            <fs_veicular>-pl_cav_prop_cnpj = <fs_zlest0194>-pl_cav_prop_cnpj.
            <fs_veicular>-pl_cav_tp_veic   = <fs_zlest0194>-pl_cav_tp_veic.


            IF  <fs_zlest0194>-frota IS INITIAL.

*-Equalização RISE x PRD - 21.08.2023 - JT - inicio
              " Busca os dados da Frota
              SELECT SINGLE frota  FROM zlest0002
                 INTO @DATA(l_frota) "(<fs_zlest0194>-frota)
                 WHERE pc_veiculo = @<fs_zlest0194>-placa_cav.

              <fs_zlest0194>-frota = l_frota.

              "Mostrar ID da Frota na tela Principal
              wa_tela-frota                  = <fs_zlest0194>-frota.
*-Equalização RISE x PRD - 21.08.2023 - JT - fim

            ENDIF.

          ENDIF.

        WHEN '2'.

          IF <fs_zlest0194>-placa_car1 IS NOT INITIAL.
            APPEND INITIAL LINE TO t_veicular  ASSIGNING FIELD-SYMBOL(<fs_veicular1>).
            <fs_veicular1>-placa_cav        = <fs_zlest0194>-placa_car1.
            <fs_veicular1>-pl_cav_prop_cod  = <fs_zlest0194>-pl_car1_prop_cod.
            <fs_veicular1>-pl_cav_prop_cnpj = <fs_zlest0194>-pl_car1_prop_cnpj.
            <fs_veicular1>-pl_cav_tp_veic   = <fs_zlest0194>-pl_car1_tp_veic.
          ENDIF.

        WHEN '3'.

          IF <fs_zlest0194>-placa_car2 IS NOT INITIAL.
            APPEND INITIAL LINE TO t_veicular  ASSIGNING FIELD-SYMBOL(<fs_veicular2>).
            <fs_veicular2>-placa_cav        = <fs_zlest0194>-placa_car2.
            <fs_veicular2>-pl_cav_prop_cod  = <fs_zlest0194>-pl_car2_prop_cod.
            <fs_veicular2>-pl_cav_prop_cnpj = <fs_zlest0194>-pl_car2_prop_cnpj.
            <fs_veicular2>-pl_cav_tp_veic   = <fs_zlest0194>-pl_car2_tp_veic.
          ENDIF.

        WHEN '4'.

          IF <fs_zlest0194>-placa_car3 IS NOT INITIAL.
            APPEND INITIAL LINE TO t_veicular  ASSIGNING FIELD-SYMBOL(<fs_veicular3>).
            <fs_veicular3>-placa_cav        = <fs_zlest0194>-placa_car3.
            <fs_veicular3>-pl_cav_prop_cod  = <fs_zlest0194>-pl_car3_prop_cod.
            <fs_veicular3>-pl_cav_prop_cnpj = <fs_zlest0194>-pl_car3_prop_cnpj.
            <fs_veicular3>-pl_cav_tp_veic   = <fs_zlest0194>-pl_car3_tp_veic.

          ENDIF.

      ENDCASE.
    ENDDO.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_CONTRATO
*&---------------------------------------------------------------------*
FORM f_preenche_contrato .

  DATA: vl_branch    TYPE j_1bbranch-branch,
        vl_tarifa    TYPE zsdt0244-tarifa,
        vl_encontrou TYPE c.

  CLEAR: t_contrato.
  READ TABLE t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>) INDEX 1.

  CHECK <fs_zlest0194>-id_ctr IS NOT INITIAL.

  SELECT *
    FROM zsdt0244
   WHERE id_ctr = @<fs_zlest0194>-id_ctr
    INTO TABLE @DATA(t_zsdt0244).

  LOOP AT t_zsdt0244 ASSIGNING FIELD-SYMBOL(<fs_zsdt0244>).

    APPEND INITIAL LINE TO t_contrato  ASSIGNING FIELD-SYMBOL(<fs_contrato>).
    <fs_contrato>-id_ctr           = <fs_zsdt0244>-id_ctr.
    <fs_contrato>-ano              = <fs_zsdt0244>-ano.
    <fs_contrato>-gr_mercadoria    = <fs_zsdt0244>-gr_mercadoria.
    <fs_contrato>-quantidade       = <fs_zsdt0244>-quantidade.
    <fs_contrato>-unid_quant       = <fs_zsdt0244>-unid_quant.
    <fs_contrato>-qtde_disponivel  = ( <fs_zsdt0244>-quantidade - <fs_zsdt0244>-total_embarque ).
    <fs_contrato>-unid_disponivel  = <fs_zsdt0244>-quantidade.
    <fs_contrato>-tarifa           = <fs_zsdt0244>-tarifa.
    <fs_contrato>-unid_tarifa      = <fs_zsdt0244>-unid_tarifa.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_LIFNR_RECEB
*&---------------------------------------------------------------------*
FORM f_busca_lifnr_receb  USING p_codigo TYPE char4
                       CHANGING p_retorno TYPE lfa1-lifnr.


  IF p_codigo = 'CNPJ'.
*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE lifnr
*      FROM lfa1
*     WHERE land1 = 'BR'
*       AND stcd1 = @wa_tela-exped_cnpj
*       AND stcd3 = @wa_tela-exped_ie
*      INTO @DATA(vl_lifnr_1).
*
*    p_retorno = vl_lifnr_1.

    SELECT *
      FROM lfa1
      WHERE land1 = 'BR'
      AND stcd1 = @wa_tela-exped_cnpj
      INTO TABLE @DATA(t_lfa1).

    LOOP AT t_lfa1 INTO DATA(lwa_lfa1).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_lfa1-stcd3
        IMPORTING
          output = lwa_lfa1-stcd3.

      CLEAR gva_lfa1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-exped_ie
        IMPORTING
          output = gva_lfa1_stcd3.

      IF lwa_lfa1-stcd3 = gva_lfa1_stcd3.
        p_retorno = lwa_lfa1-lifnr.
      ENDIF.

      CLEAR: lwa_lfa1.
    ENDLOOP.
*** BUG - 89608 - Fim - CBRAND

  ELSEIF p_codigo = 'CPF'.
*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE lifnr
*      FROM lfa1
*     WHERE land1 = 'BR'
*       AND stcd2 = @wa_tela-exped_cpf
*       AND stcd3 = @wa_tela-exped_ie
*      INTO @DATA(vl_lifnr_2).
*
*    p_retorno = vl_lifnr_2.

    CLEAR:  t_lfa1, lwa_lfa1.

    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1
      WHERE land1 = 'BR'
      AND stcd2 = wa_tela-exped_cpf.


    LOOP AT t_lfa1 INTO lwa_lfa1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_lfa1-stcd3
        IMPORTING
          output = lwa_lfa1-stcd3.

      CLEAR gva_lfa1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-exped_ie
        IMPORTING
          output = gva_lfa1_stcd3.

      IF lwa_lfa1-stcd3 = gva_lfa1_stcd3.
        p_retorno = lwa_lfa1-lifnr.
      ENDIF.

      CLEAR: lwa_lfa1.
    ENDLOOP.
*** BUG - 89608 - Inicio - CBRAND

*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
  ELSE.

    IF wa_tela-exped_rsocial IS INITIAL.

      wa_tela-exped_rsocial = wa_tela-reme_rsocial.
      wa_tela-exped_cnpj    = wa_tela-reme_cnpj.
      wa_tela-exped_cpf     = wa_tela-reme_cpf.
      wa_tela-exped_ie      = wa_tela-reme_ie.
      p_retorno             = wa_tela-reme_cod_forn.

    ENDIF.
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Fim de Alteração

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_KUNNR_RECEB
*&---------------------------------------------------------------------*
FORM f_busca_kunnr_receb   USING p_codigo  TYPE char4
                        CHANGING p_retorno TYPE kna1-kunnr.

  IF p_codigo = 'CNPJ'.

*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE kunnr
*        FROM kna1
*       WHERE land1 = 'BR'
*         AND stcd1 = @wa_tela-receb_cnpj
*         AND stcd3 = @wa_tela-receb_ie
*        INTO @DATA(vl_kunnr_1).
*
*    p_retorno = vl_kunnr_1.

    SELECT *
      FROM kna1
     WHERE land1 = 'BR'
       AND stcd1 = @wa_tela-receb_cnpj
      INTO TABLE @DATA(t_kna2).

    LOOP AT t_kna2 INTO DATA(lwa_kna2).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_kna2-stcd3
        IMPORTING
          output = lwa_kna2-stcd3.

      CLEAR gva_kna1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-receb_ie
        IMPORTING
          output = gva_kna1_stcd3.

      IF lwa_kna2-stcd3 = gva_kna1_stcd3.
        p_retorno = lwa_kna2-kunnr.
      ENDIF.

      CLEAR: lwa_kna2.
    ENDLOOP.
*** BUG - 89608 - Fim - CBRAND

  ELSEIF p_codigo = 'CPF'.

*** BUG - 89608 - Inicio - CBRAND
*    SELECT SINGLE kunnr
*        FROM kna1
*       WHERE land1 = 'BR'
*         AND stcd2 = @wa_tela-receb_cpf
*         AND stcd3 = @wa_tela-receb_ie
*        INTO @DATA(vl_kunnr_2).
*
*    p_retorno = vl_kunnr_2.

    CLEAR:  t_kna2, lwa_kna2.
    SELECT *
      FROM kna1
     INTO TABLE t_kna2
   WHERE land1 = 'BR'
     AND stcd2 = wa_tela-receb_cpf
     AND loevm <> abap_true. "FF #178888.


    LOOP AT t_kna2 INTO lwa_kna2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_kna2-stcd3
        IMPORTING
          output = lwa_kna2-stcd3.

      CLEAR gva_kna1_stcd3.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_tela-receb_ie
        IMPORTING
          output = gva_kna1_stcd3.

      IF lwa_kna2-stcd3 = gva_kna1_stcd3.
        p_retorno = lwa_kna2-kunnr.
      ENDIF.

      CLEAR: lwa_kna2.
    ENDLOOP.
*** BUG - 89608 - Fim - CBRAND

*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
  ELSE.

    IF wa_tela-receb_rsocial IS INITIAL.

      wa_tela-receb_rsocial = wa_tela-dest_rsocial.
      wa_tela-receb_cnpj    = wa_tela-dest_cnpj.
      wa_tela-receb_cpf     = wa_tela-dest_cpf.
      wa_tela-receb_ie      = wa_tela-dest_ie.
      p_retorno             = wa_tela-dest_cod_cliente.

    ENDIF.
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Fim de Alteração

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_MOTORISTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_motorista INPUT.
  PERFORM zf_preencher_compl_motorista.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_FROTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_frota INPUT.
  PERFORM f_buscar_placas_frota.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_PLACAS_FROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_buscar_placas_frota .

  DATA: r_frota TYPE RANGE OF zlest0002-frota.

  IF wa_tela-frota IS NOT INITIAL.

    REFRESH: t_veicular.

    r_frota = VALUE #( ( sign = 'I' option = 'EQ' low = wa_tela-frota ) ).

* Seleção para  conjunto veicular
    SELECT  *
      FROM zlest0002
     WHERE frota IN @r_frota
      INTO TABLE @DATA(t_frota).

    IF t_frota[] IS  INITIAL.
      MESSAGE s000(z_les) WITH 'Nenhum conjunto veicular encontrado'
                                'para a frota informada!' DISPLAY LIKE 'E'.
      RETURN.
    ELSE.

      SELECT lifnr, stcd1, stcd2 FROM lfa1
       INTO TABLE @DATA(t_prop_veic)
        FOR ALL ENTRIES IN @t_frota
       WHERE land1 = 'BR'
         AND lifnr = @t_frota-proprietario.

    ENDIF.


    LOOP AT t_frota INTO DATA(w_frota).

      wa_veicular-placa_cav           = w_frota-pc_veiculo.
      wa_veicular-pl_cav_prop_cod     = w_frota-proprietario.
      wa_veicular-pl_cav_tp_veic      = w_frota-ct_veiculo.

      READ TABLE t_prop_veic INTO DATA(w_prop_veic) WITH KEY lifnr = w_frota-proprietario.

      CHECK sy-subrc IS INITIAL.

      IF w_prop_veic-stcd1 IS NOT INITIAL.
        wa_veicular-pl_cav_prop_cnpj = w_prop_veic-stcd1.
      ELSEIF w_prop_veic-stcd2 IS NOT INITIAL.
        wa_veicular-pl_cav_prop_cnpj = w_prop_veic-stcd2.
      ENDIF.

      APPEND wa_veicular TO t_veicular.

    ENDLOOP.

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHER_COMPL_MOTORISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preencher_compl_motorista .

*&---------------------------------------------------------------------*
* Seleção para  motorista
*&---------------------------------------------------------------------*
  DATA: r_lifnr TYPE RANGE OF lfa1-lifnr,
        r_stcd2 TYPE RANGE OF lfa1-stcd2.

  DATA(l_mot_cpf) = wa_tela-mot_cpf.

  IF wa_tela-mot_cpf IS NOT INITIAL.
    r_stcd2 = VALUE #( ( sign = 'I' option = 'EQ' low = wa_tela-mot_cpf ) ).
  ENDIF.

  IF r_stcd2[] IS NOT INITIAL.

    CLEAR: wa_tela-motorista, wa_tela-nome_motorista, wa_tela-mot_cpf.
    SELECT SINGLE lifnr, name1, stcd2
      FROM lfa1
     WHERE land1 = 'BR'
       AND stcd2 IN @r_stcd2
       AND ktokk = 'ZMOT'
      INTO @DATA(vl_lfa1).

    IF sy-subrc NE 0.
      MESSAGE s000(z_les) WITH 'CPF' l_mot_cpf
                               'do motorista não encontrado para o'
                               TEXT-011 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF v_motorista_cod_sap IS INITIAL.
      wa_tela-motorista = vl_lfa1-lifnr.
    ENDIF.

    wa_tela-nome_motorista = vl_lfa1-name1.
    wa_tela-mot_cpf        = vl_lfa1-stcd2.
    wa_tela-motorista      = vl_lfa1-lifnr. "v_motorista_cod_sap.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_ATUALIZAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_atualizar INPUT.
  PERFORM f_atualizar.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_MOTORISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_buscar_motorista.

  DATA: r_stcd2 TYPE RANGE OF lfa1-stcd2,
        r_lifnr TYPE RANGE OF lfa1-lifnr.

  DATA: l_motorista_cpf TYPE lfa1-stcd2.

  l_motorista_cpf = wa_tela-mot_cpf.

  IF   l_motorista_cpf IS NOT INITIAL.
    r_stcd2 = VALUE #( ( sign = 'I' option = 'EQ' low = wa_tela-mot_cpf ) ).
  ENDIF.

  IF r_stcd2[] IS NOT INITIAL.

* Seleção para  motorista
    SELECT SINGLE lifnr, name1, stcd2
      FROM lfa1
     WHERE land1 = 'BR'
       AND lifnr IN @r_lifnr  "v_motorista_cod_sap
       AND stcd2 IN @r_stcd2
       AND ktokk = 'ZMOT'
      INTO @DATA(wa_lfa1).

    IF sy-subrc NE 0.

      CLEAR: wa_tela-nome_motorista, wa_tela-motorista.
      MESSAGE s000(z_les) WITH TEXT-010 TEXT-011 DISPLAY LIKE 'S'.
      RETURN.

    ELSE.

      wa_tela-nome_motorista = wa_lfa1-name1.
      wa_tela-mot_cpf        = wa_lfa1-stcd2.
      wa_tela-motorista      = wa_lfa1-lifnr.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_selecionar_contrato .

  DATA: vl_branch TYPE j_1bbranch-branch,
        vl_tarifa TYPE zsdt0244-tarifa.

  TYPES: BEGIN OF y_poupup.
           INCLUDE STRUCTURE zsds024.
  TYPES: END OF y_poupup.

  DATA: t_return_tab TYPE TABLE OF ddshretval,
        t_dselc      TYPE TABLE OF dselc,
        t_poupup     TYPE TABLE OF y_poupup.

  DATA: wa_poupup LIKE LINE OF t_poupup.

  CLEAR: t_return_tab, t_dselc.

  REFRESH  t_poupup.
  CHECK wa_tela-chave_xml_cte IS NOT INITIAL.

  IF t_veicular[] IS INITIAL.
    MESSAGE s000(z_les) WITH 'Nenhum conjunto veicular encontrado' 'para a frota informada!'
      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Para buscado contrato utiliza-se a placa cavalo
  READ TABLE t_veicular ASSIGNING FIELD-SYMBOL(<fs_veicular>)
                                       WITH KEY pl_cav_tp_veic = 1.
  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_veicular>-pl_cav_prop_cod+6(4)
      IMPORTING
        output = vl_branch.

    "Verifica Filial para encontrar empresa
    SELECT SINGLE *
      FROM j_1bbranch
     WHERE branch = @vl_branch
      INTO @DATA(wa_j_1bbranch).

    "Busca contratos disponíveis
    SELECT * FROM zsdt0244
    WHERE bukrs       = @wa_j_1bbranch-bukrs
      AND id_cliente  = @wa_tela-emit_cod_cliente "Emitente Cliente
      "AND pc_codigo  = @wa_tela-reme_cod_forn US - 82085 - CBRAND
      AND pc_codigo   = @wa_tela-exped_cod_forn    "US - 82085 - CBRAND - "Remetente / Ponto Coleta
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
*      AND lr_codigo   = @wa_tela-dest_cod_cliente  "Destinatário Código Cliente
*      AND dest_codigo = @wa_tela-receb_cod_cliente "BUG - 89539 - CBRAND - "Destinatário Local Entrega
      AND lr_codigo   = @wa_tela-receb_cod_cliente
      AND dest_codigo = @wa_tela-dest_cod_cliente
*** Stefanini - IR201617 - 16/10/2024 - LAZAROSR - Início de Alteração
      AND status      = '0001'
     INTO TABLE @DATA(t_zsdt0244).

    IF t_zsdt0244[] IS INITIAL.
      MESSAGE s000(z_les) WITH 'Nenhum contrato encontrado' 'para o Emitente\Cliente!'
      DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT t_zsdt0244 ASSIGNING FIELD-SYMBOL(<fs_zsdt0244>).

      "Verifica se o contrato têm quantidade disponível
      DATA(l_tol_max) = ( <fs_zsdt0244>-quantidade + ( <fs_zsdt0244>-quantidade * <fs_zsdt0244>-tolerancia ) / 100 ).

      "Quantidade disponível
      l_tol_max = l_tol_max - <fs_zsdt0244>-total_embarque.

      CHECK l_tol_max >= wa_tela-qt_carga_cte.

      wa_poupup-id_ctr           = <fs_zsdt0244>-id_ctr.
      wa_poupup-ano              = <fs_zsdt0244>-ano.
      wa_poupup-gr_mercadoria    = <fs_zsdt0244>-gr_mercadoria.
      wa_poupup-quantidade       = <fs_zsdt0244>-quantidade.
      wa_poupup-unid_quant       = <fs_zsdt0244>-unid_quant.
      wa_poupup-qtde_disponivel  = ( <fs_zsdt0244>-total_embarque ).
      wa_poupup-unid_disponivel  = <fs_zsdt0244>-unid_quant.
      wa_poupup-tarifa           = <fs_zsdt0244>-tarifa.
      wa_poupup-unid_tarifa      = <fs_zsdt0244>-unid_tarifa.

      APPEND wa_poupup TO t_poupup.
      CLEAR wa_poupup.

    ENDLOOP.

    IF t_poupup[] IS INITIAL.
      REFRESH: t_zsdt0244.
      MESSAGE s000(z_les) WITH 'Nenhum contrato encontrado com peso' 'disponível para o Emitente\Cliente!'
      DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CHECK t_zsdt0244[] IS NOT INITIAL.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'ID_CTR'
        value_org       = 'S'
      TABLES
        value_tab       = t_poupup
        return_tab      = t_return_tab
        dynpfld_mapping = t_dselc.


    READ TABLE t_return_tab INTO DATA(wa_return_tab) WITH KEY fieldname = 'F0001'.

    IF wa_return_tab IS NOT INITIAL.

      REFRESH: t_contrato.

      "Preenche contrato da tela
      SELECT id_ctr ano gr_mercadoria quantidade unid_quant
         total_embarque unid_embarque tarifa unid_tarifa
           FROM zsdt0244
           INTO TABLE  t_contrato
          WHERE id_ctr   = wa_return_tab-fieldval.

*** US - 82085 - Inicio - CBRAND
*** REGRA para remetente fora do brasil
      IF wa_tela-reme_cnpj IS INITIAL AND wa_tela-reme_cpf IS INITIAL.
        SELECT SINGLE reme_codigo
         FROM zsdt0244
          INTO wa_tela-reme_cod_forn
           WHERE id_ctr   = wa_return_tab-fieldval.
      ENDIF.
*** US - 82085 - Fim - CBRAND

    ENDIF.

  ENDIF.

ENDFORM.

MODULE criar_objetos_0200 OUTPUT.

  IF wa_tela-chave_xml_cte IS NOT INITIAL.

    "IF WG_ACAO EQ C_VIEW_DOC.
    "  IP_MODE = 'D'.
    "ELSE.
    ip_mode = 'E'.
    "ENDIF.

    CLEAR obj.
    obj-objtype = objtype.

    obj-objkey = wa_tela-chave_xml_cte.

    CREATE OBJECT manager
      EXPORTING
        is_object        = obj
        ip_no_commit     = 'R'
        ip_mode          = ip_mode
      EXCEPTIONS
        object_invalid   = 1
        callback_invalid = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDMODULE.


*FORM f_importar_anexos.
*
*  DATA: folder_id TYPE sofdk,
*        owner_dat TYPE soud3,
*        vl_error  TYPE c.
*
*  DATA: it_file_table    TYPE filetable,
*        wa_file_table   TYPE file_table,
*        lc_rc            TYPE i,
*        vl_file_table_c TYPE string.
*
*  "READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
*  "CHECK sy-subrc = 0.
*  "READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
*  "CHECK sy-subrc = 0.
*
*  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
*    EXPORTING
*      region    = 'B'
*    IMPORTING
*      folder_id = folder_id
*    EXCEPTIONS
*      OTHERS    = 0.
*
*  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*    EXPORTING
*      window_title            = 'Importar Arquivo '
*      file_filter             = 'Files PDF (*.PDF)|*.PDF|'
*      multiselection          = abap_true
*      initial_directory       = 'C:\Amaggi\'
*    CHANGING
*      file_table              = it_file_table
*      rc                      = lc_rc
*    EXCEPTIONS
*      file_open_dialog_failed = 1
*      cntl_error              = 2
*      error_no_gui            = 3
*      not_supported_by_gui    = 4
*      OTHERS                  = 5.
*
*  IF sy-subrc IS NOT INITIAL.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    RETURN.
*  ENDIF.
*
*  CLEAR: vl_error.
*  LOOP AT it_file_table INTO wa_file_table.
*    vl_file_table_c = wa_file_table.
*    PERFORM f_create_anexos USING folder_id vl_file_table_c vl_error.
*    IF vl_error IS NOT INITIAL.
*      RETURN.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.



*FORM f_create_anexos  USING  p_folder_id     TYPE sofdk
*                             p_path_and_file TYPE string
*                    CHANGING p_error.
*
*  MESSAGE s003(vpd).
*
*
*  DATA: back LIKE sy-ucomm VALUE 'BAC',  " Back
*        canc LIKE sy-ucomm VALUE 'ESC',  " Cancel
*        stop LIKE sy-ucomm VALUE 'RET'.  " Terminate SAPoffice
*
*  DATA is_object  TYPE borident.
*  DATA attachment TYPE borident.
*  DATA documents  TYPE STANDARD TABLE OF sood4.
*
*  DATA document   TYPE sood4.
*  DATA ep_attachment TYPE char40.
*
*  DATA l_cancelled   LIKE sonv-flag.
*  DATA bin_filesize  LIKE soxwd-doc_length.
*  DATA file_format   LIKE rlgrap-filetype.
*  DATA path_and_file TYPE string.
*  DATA object_type   LIKE soodk-objtp.
*  DATA put_to_kpro   LIKE sonv-flag.
*  DATA p_objcont     TYPE TABLE OF soli.
*  DATA p_objhead     TYPE TABLE OF soli.
*  DATA p_objpara     TYPE TABLE OF selc.
*  DATA p_objparb     TYPE TABLE OF soop1.
*
*  DATA doc_id      LIKE soodk.
*  DATA att_id      LIKE soodk.
*  DATA hd_dat      LIKE sood1.
*  DATA fm_dat      LIKE sofm1.
*  DATA new_doc_id  LIKE soodk.
*  DATA new_att_id  LIKE soodk.
*  DATA new_hd_dat  LIKE sood2.
*  DATA new_fm_dat  LIKE sofm2.
*  DATA old_doc_id  LIKE soodk.
*  DATA fol_dat     LIKE sofdd.
*  DATA old_enccnt  LIKE sood-enccnt.
*  DATA attach_list LIKE sood5 OCCURS 0 WITH HEADER LINE.
*  DATA link_list   LIKE soodk OCCURS 0 WITH HEADER LINE.
*  DATA l_reappear  LIKE sonv-flag.
*  DATA l_answer.
*  DATA l_filename           TYPE string.
*  DATA reference_type_kpro VALUE 'K'.      "KPro reference
*  DATA obj_type             LIKE soodk-objtp.
*  DATA owner_dat            LIKE soud3.
*
*
*
*  DATA  p_header_data LIKE sood2.
*  DATA  p_folmem_data LIKE sofm2.
*  DATA  l_name TYPE string.                                 "1041757 >>
*  DATA  g_document     LIKE sood4.
*  DATA  lo_objhead TYPE REF TO cl_bcs_objhead.
*
*  IF owner IS INITIAL.
*    CLEAR owner_dat.
*    MOVE sy-uname TO owner_dat-sapnam.
*    CALL FUNCTION 'SO_NAME_CONVERT'
*      EXPORTING
*        name_in               = owner_dat
*        no_address_name       = 'X'
*      IMPORTING
*        name_out              = owner_dat
*      EXCEPTIONS
*        communication_failure = 71
*        office_name_not_exist = 19
*        parameter_error       = 23
*        sap_name_not_exist    = 29
*        system_failure        = 72
*        user_not_exist        = 34.
*    IF sy-subrc NE 0.
*      p_error = 'X'.
*      RETURN.
*    ENDIF.
*
*    MOVE owner_dat-usrnam TO owner.
*
*    CALL FUNCTION 'SO_FOLDER_HEADER_READ'
*      EXPORTING
*        folder_id                  = p_folder_id
*        owner                      = owner
*      IMPORTING
*        folder_data                = sofd_dat
*      EXCEPTIONS
*        communication_failure      = 71
*        folder_not_exist           = 6
*        operation_no_authorization = 21
*        system_failure             = 72.
*
*    IF sy-subrc NE 0.
*      p_error = 'X'.
*      RETURN.
*    ENDIF.
*
*  ENDIF.
*
*  g_document-foltp = p_folder_id-foltp.
*  g_document-folyr = p_folder_id-folyr.
*  g_document-folno = p_folder_id-folno.
*  g_document-folrg = sofd_dat-folrg.
*
*  CALL FUNCTION 'SO_OBJECT_UPLOAD'
*    EXPORTING
*      filetype                = 'BIN'
*      path_and_file           = p_path_and_file
*      no_dialog               = 'X'
*    IMPORTING
*      f_cancelled             = l_cancelled
*      filelength              = bin_filesize
*      act_filetype            = file_format
*      act_filename            = path_and_file
*      act_objtype             = object_type
*      file_put_to_kpro        = put_to_kpro
*    TABLES
*      objcont                 = p_objcont
*    EXCEPTIONS
*      invalid_type            = 1
*      object_type_not_allowed = 2
*      kpro_insert_error       = 3
*      file_to_large           = 4
*      OTHERS                  = 5.
*
*  CASE sy-subrc.
*    WHEN 0.
*      IF NOT l_cancelled IS INITIAL.
*        MESSAGE s118(so). g_document-okcode = canc. EXIT.
*      ENDIF.
*      hd_dat-objlen = bin_filesize.
*
*      IF NOT put_to_kpro IS INITIAL.
*        hd_dat-extct = reference_type_kpro.
*      ENDIF.
*
*      CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
*        EXPORTING
*          full_name     = path_and_file
*        IMPORTING
*          stripped_name = l_filename.
*
*      CLEAR: p_objhead, p_objhead[].
*      lo_objhead = cl_bcs_objhead=>create( p_objhead[] ).
*      lo_objhead->set_filename( l_filename ).
*      lo_objhead->set_format( file_format ).
*      lo_objhead->set_vsi_profile( cl_bcs_vsi_profile=>get_profile( ) ).
*      p_objhead[] = lo_objhead->mt_objhead.
*      hd_dat-file_ext = object_type.
*
*      "if hd_dat-objdes is initial.
***       split l_filename at '.' into hd_dat-objdes l_filename.
*      PERFORM so_split_file_and_extension(saplso30)
*                                          USING l_filename
*                                                hd_dat-objdes
*                                                object_type.
*      g_document-objdes = hd_dat-objdes.
*      "endif.
*
*    WHEN 1.
*      p_error = 'X'.
*      MESSAGE i422(so) WITH file_format. g_document-okcode = canc.
*      EXIT.
*    WHEN 2.
*      p_error = 'X'.
*      MESSAGE i322(so). g_document-okcode = canc.
*      EXIT.
*    WHEN 3.
*      p_error = 'X'.
*      MESSAGE i444(so). g_document-okcode = canc.
*      EXIT.
*    WHEN 4.
*      p_error = 'X'.
*      MESSAGE i425(so). g_document-okcode = canc.
*      EXIT.
*    WHEN 5.
*      p_error = 'X'.
*      DATA lv_done TYPE c.
*      lv_done = cl_bcs_vsi_profile=>output_vsi_error( ).
*      IF lv_done IS INITIAL.
*        MESSAGE i424(so).
*      ENDIF.
*      g_document-okcode = 'ESC'.
*      EXIT.
*  ENDCASE.
*
*  hd_dat-file_ext = object_type.
*  obj_type = 'EXT'.
*
*  CALL FUNCTION 'SO_OBJECT_INSERT'
*    EXPORTING
*      folder_id                  = p_folder_id
*      object_type                = obj_type
*      object_hd_change           = hd_dat
*      object_fl_change           = fm_dat
*      owner                      = owner
*    IMPORTING
*      object_id                  = new_doc_id
*      object_hd_display          = new_hd_dat
*      object_fl_display          = new_fm_dat
*    TABLES
*      objcont                    = p_objcont
*      objhead                    = p_objhead
*      objpara                    = p_objpara
*      objparb                    = p_objparb
*    EXCEPTIONS
*      active_user_not_exist      = 35
*      communication_failure      = 71
*      component_not_available    = 1
*      dl_name_exist              = 3
*      folder_no_authorization    = 5
*      folder_not_exist           = 6
*      object_type_not_exist      = 17
*      operation_no_authorization = 21
*      owner_not_exist            = 22
*      parameter_error            = 23
*      substitute_not_active      = 31
*      substitute_not_defined     = 32
*      system_failure             = 72
*      x_error                    = 1000.
*
*  IF sy-subrc > 0.
*    p_error = 'X'.
*    RETURN.
*  ELSE.
*    MOVE: new_doc_id-objtp TO g_document-objtp,
*          new_doc_id-objyr TO g_document-objyr,
*          new_doc_id-objno TO g_document-objno,
*          new_hd_dat       TO p_header_data,
*          new_fm_dat       TO p_folmem_data.
*    g_document-objnam = new_hd_dat-objnam.
*    g_document-objdes = new_hd_dat-objdes.
*    g_document-okcode = 'CREA'.
*    IF sy-batch IS INITIAL AND sy-binpt IS INITIAL.
*      MESSAGE s109(so).
*    ENDIF.
*  ENDIF.
*
*  is_object-objtype = 'ZLES0180'.
*  is_object-objkey  = WA_TELA-CHAVE_XML_CTE.
*
*  IF g_document-okcode = 'CREA' OR g_document-okcode = 'CHNG'.
*    attachment-objtype = 'MESSAGE'.
*    attachment-objkey  =  g_document(34).
*    CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
*      EXPORTING
*        obj_rolea    = is_object
*        obj_roleb    = attachment
*        relationtype = 'ATTA'
*      EXCEPTIONS
*        OTHERS       = 1.
*    IF sy-subrc = 0.
*      MESSAGE 'Arquivo(s) Anexado(s)' TYPE 'S'.
*    ENDIF.
*  ENDIF.
*
*
*
*
*ENDFORM.
