*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: Aquaviário                                              &*
*& Data.....: 08/11/2013                                              &*
*& Descrição: Frete Aquaviário                                        &*
*&--------------------------------------------------------------------&*
REPORT  zlesr0075.

CLASS cl_gui_cfw DEFINITION LOAD.
**&--------------------------------------------------------------------&*
**& TYPES
**&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_tela_0100,
         wp_bukrs       TYPE t001-bukrs, "Empresa
         wp_werks       TYPE t001w-werks, "Centro Emissor
         wp_ano         TYPE zlest0056-ano_viagem, "Ano da Viagem
         wp_viagem      TYPE zlest0058-nr_viagem, "Número da Viagem
         w_desc_bukrs   TYPE t001-butxt,  "Descrição da Empresa
         w_desc_werks   TYPE t001w-name1, "Descrição do Centro
         wp_barcaca     TYPE c LENGTH 1,  "Layout para vizualização por barcaça
         wp_cte         TYPE c LENGTH 1,  "Layout para vizualização por Cte
         wp_nfe         TYPE c LENGTH 1,  "Layout para vizualização por NF-e
         operacao       TYPE zlest0060-operacao,
         dt_sai_comboio TYPE erdat,
         hr_sai_comboio TYPE erzet,
       END OF ty_tela_0100,

       BEGIN OF ty_zlest0060,
         bukrs           TYPE zlest0060-bukrs,
         werks           TYPE zlest0060-werks,
         nr_viagem       TYPE zlest0060-nr_viagem,
         ano_viagem      TYPE zlest0060-ano_viagem,
         peso_fiscal     TYPE zlest0060-peso_fiscal,
         peso_chegada    TYPE zlest0060-peso_fiscal,
         data_chegada    TYPE zlest0061-dt_chegada,
         ck_anulado      TYPE zlest0061-ck_anulado,
         hr_prevista     TYPE zlest0056-hr_prevista,
         docnum          TYPE zlest0060-docnum,
         nome_emb        TYPE zlest0060-nome_emb,
         dt_descarga_ini TYPE zlest0060-dt_descarga_ini,
         hr_descarga_ini TYPE zlest0060-hr_descarga_ini,
         dt_descarga_fim TYPE zlest0060-dt_descarga_fim,
         hr_descarga_fim TYPE zlest0060-hr_descarga_fim,
       END OF ty_zlest0060,

       BEGIN OF ty_saida_viagem,
         nome_emb            TYPE zlest0063-nome_emb,
         cod_material        TYPE zlest0063-cod_material,
         maktx               TYPE makt-maktx,
         ano_viagem          TYPE zlest0063-ano_viagem,
         bukrs               TYPE zlest0063-bukrs,
         werks               TYPE zlest0063-werks,
         nr_viagem           TYPE zlest0063-nr_viagem,
         tp_class            TYPE c LENGTH 15,
*"// wbarbosa 16102024 US-153329
         ico_eudr            TYPE c LENGTH 4,
*"// wbarbosa 16102024 US-153329
         peso_faturado       TYPE zlest0061-peso_vinculado,
         data_chegada        TYPE zlest0061-dt_chegada,
         hr_prevista         TYPE zlest0056-hr_prevista,
         dt_descarga_ini     TYPE zlest0061-dt_descarga_ini,
         hr_descarga_ini     TYPE zlest0061-hr_descarga_ini,
         dt_descarga_fim     TYPE zlest0061-dt_descarga_fim,
         hr_descarga_fim     TYPE zlest0061-hr_descarga_fim,
         peso_descarga       TYPE zde_peso_chegada_sdec,
         ck_anulado          TYPE zlest0061-ck_anulado,
         ck_terminal         TYPE c LENGTH 1,
         dt_chegada_terminal TYPE zlest0061-dt_chegada_terminal,
         visu                TYPE c LENGTH 4,
         edit                TYPE c LENGTH 4,
         estilo              TYPE lvc_t_styl,
       END OF ty_saida_viagem,

       BEGIN OF ty_organiza_tree,
         nfe_cte     TYPE znfnum,
         cl_codigo   TYPE j_1bparid,
         nfnum       TYPE znfnum,
         series      TYPE j_1bseries,
         peso_fiscal TYPE brgew,
         netwr       TYPE netwr,
         nr_romaneio TYPE znr_romaneio,
         safra       TYPE znr_safra,
         nr_dco      TYPE zdoc_rem,
         docnum      TYPE j_1bdocnum,
       END OF ty_organiza_tree,

       BEGIN OF ty_saida_tree,
         cl_codigo   TYPE j_1bparid,
         nfnum       TYPE znfnum,
         series      TYPE j_1bseries,
         peso_fiscal TYPE brgew,
         netwr       TYPE netwr,
         nr_romaneio TYPE znr_romaneio,
         safra       TYPE znr_safra,
         nr_dco      TYPE zdoc_rem,
         docnum      TYPE j_1bdocnum,
       END OF ty_saida_tree,

       BEGIN OF ty_saida_nr_viagem,
         bukrs      TYPE zlest0058-bukrs,
         werks      TYPE zlest0058-werks,
         ano_viagem TYPE zlest0058-ano_viagem,
         nr_viagem  TYPE zlest0058-nr_viagem,
       END OF ty_saida_nr_viagem,

       BEGIN OF ty_saida_cte,
         nfenum        TYPE znfnum,
         cl_codigo     TYPE j_1bparid,
         peso_fiscal   TYPE brgew,
         peso_chegada  TYPE brgew,
         nome_emb      TYPE zlest0063-nome_emb,
         cod_material  TYPE zlest0063-cod_material,
         maktx         TYPE makt-maktx,
         ano_viagem    TYPE zlest0063-ano_viagem,
         bukrs         TYPE zlest0063-bukrs,
         werks         TYPE zlest0063-werks,
         nr_viagem     TYPE zlest0063-nr_viagem,
         tp_class      TYPE c LENGTH 15,
         peso_faturado TYPE zlest0061-peso_vinculado,
         data_chegada  TYPE zlest0061-dt_chegada,
         hr_prevista   TYPE zlest0056-hr_prevista,
         peso_descarga TYPE zlest0061-peso_chegada,
         data_fatura   TYPE zlest0061-dt_fatura,
         ck_anulado    TYPE zlest0061-ck_anulado,
         docnum        TYPE zlest0060-docnum,
*"// wbarbosa 16102024 US-153329
         ico_eudr      TYPE c LENGTH 4,
*"// wbarbosa 16102024 US-153329
       END OF ty_saida_cte,

       BEGIN OF ty_saida_nfe,
         nfnum         TYPE zlest0060-nfnum,
         docdat        TYPE zlest0060-docdat,
         chave_nfe     TYPE zlest0060-chave_nfe,
         dt_chegada    TYPE zlest0060-dt_chegada,
         hr_chegada    TYPE zlest0060-hr_chegada,
         peso_chegada  TYPE zlest0060-peso_chegada,
         peso_fiscal   TYPE zlest0060-peso_fiscal,
         ch_referencia TYPE zlest0060-ch_referencia,
         tp_transgenia TYPE c LENGTH 15,
         rm_codigo     TYPE zlest0060-rm_codigo,
         name1         TYPE lfa1-name1,
         matnr         TYPE matnr,
         maktx         TYPE makt-maktx,
         nome_emb      TYPE zlest0063-nome_emb,
         num_cte       TYPE znfnum,
         vinc_tot      TYPE c LENGTH 4,
         disp_cct      TYPE c LENGTH 4,
         recep_cct     TYPE c LENGTH 4,
*"// wbarbosa 16102024 US-153329
         ico_eudr      TYPE c LENGTH 4,
*"// wbarbosa 16102024 US-153329
         seq_lcto_ind  TYPE zfiwrt0008-seq_lcto,
         docnum_ind    TYPE j_1bdocnum,
         nfenum_ind    TYPE j_1bnfdoc-nfenum,

         belnr_ind     TYPE zib_contabil_chv-belnr,

         estilo        TYPE lvc_t_styl,
       END OF ty_saida_nfe.

DATA: r_matnr TYPE RANGE OF mara-matnr.

DATA: nfnum TYPE zlest0060-nfnum.
DATA: answer TYPE char1 VALUE IS INITIAL.
* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: zcl_token_siscomex    TYPE REF TO zcl_token_siscomex.

*"// WBARBOSA 16102024 US-153329
CONSTANTS: gc_atende_eudr TYPE c LENGTH 01 VALUE 'S'.
*"// WBARBOSA 16102024 US-153329

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION


*&--------------------------------------------------------------------&*
*& DATA
*&--------------------------------------------------------------------&*
DATA: obj_custom_viagem TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da Viagem
      obj_grid_viagem   TYPE REF TO cl_gui_alv_grid, "Classe referente ao Container da Viagem
      obj_toolbar       TYPE REF TO lcl_alv_toolbar.

DATA: obj_custom_tree TYPE REF TO cl_gui_custom_container,
      obj_grid_tree   TYPE REF TO cl_gui_alv_tree.

DATA: gt_catalog_tree TYPE lvc_t_fcat,
      gt_header       TYPE treev_hhdr.

DATA: obj_custom_nr_viagem TYPE REF TO cl_gui_custom_container,
      obj_grid_nr_viagem   TYPE REF TO cl_gui_alv_grid.

DATA: gt_fcat_viagem TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Viagem
      gw_fcat_viagem TYPE lvc_s_fcat. "Controle VLA: catálogo de campos -  Viagem


DATA: gt_fcat_nr_viagem TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Viagem
      gw_fcat_nr_viagem TYPE lvc_s_fcat. "Controle VLA: catálogo de campos -  Viagem

DATA: gt_fcat_cte TYPE lvc_t_fcat,
      gw_fcat_cte TYPE lvc_s_fcat.

DATA: obj_custom_cte     TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da Viagem
      obj_grid_cte       TYPE REF TO cl_gui_alv_grid, "Classe referente ao Container da Viagem
      zcl_cct_control_nf TYPE REF TO zcl_cct_control_nf.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      gw_estilo TYPE lvc_s_styl.

DATA: wa_stable       TYPE lvc_s_stbl VALUE 'XX'.

*&--------------------------------------------------------------------&*
*& SHDB                                                               &*
*&--------------------------------------------------------------------&*
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF ti_bdcdata,
      tl_bdc     TYPE TABLE OF bdcdata,
      wl_bdc     TYPE bdcdata,
      opt        TYPE ctu_params.


*&--------------------------------------------------------------------&*
*& Internal Table
*&--------------------------------------------------------------------&*
DATA: gt_saida_viagem    TYPE TABLE OF ty_saida_viagem,
      gt_organiza_tree   TYPE TABLE OF ty_organiza_tree,
      gt_saida_nr_viagem TYPE TABLE OF ty_saida_nr_viagem,
      gt_saida_cte       TYPE TABLE OF ty_saida_cte,
      gt_saida_nfe       TYPE TABLE OF ty_saida_nfe,
      gt_saida_tree      TYPE TABLE OF ty_saida_tree,
      gt_zlest0060       TYPE TABLE OF ty_zlest0060.

*&--------------------------------------------------------------------&*
*& Work Area
*&--------------------------------------------------------------------&*
DATA: gw_saida_viagem    TYPE ty_saida_viagem,
      gw_organiza_tree   TYPE ty_organiza_tree,
      gw_saida_tree      TYPE ty_saida_tree,
      gw_saida_nr_viagem TYPE ty_saida_nr_viagem,
      gw_saida_cte       TYPE ty_saida_cte,
      gw_saida_nfe       TYPE ty_saida_nfe,
      gw_zlest0060       TYPE ty_zlest0060.
*&--------------------------------------------------------------------&*
*& DATA
*&--------------------------------------------------------------------&*
DATA: wp_tela_0100 TYPE ty_tela_0100.
*&--------------------------------------------------------------------&*
*& CONSTANTES
*&--------------------------------------------------------------------&*
CONSTANTS: tela_0100 TYPE sy-dynnr VALUE '0100'. "Tela Principal
*&--------------------------------------------------------------------&*
*& VARIAVEIS
*&--------------------------------------------------------------------&*
DATA: ok_code         TYPE sy-ucomm. "Código de função que acionou o PAI
DATA: var_title_popup TYPE c LENGTH 200.
DATA: var_tp_alv      TYPE c LENGTH 1.
DATA: vg_dt_sai_comboio TYPE erdat,
      vg_hr_sai_comboio TYPE erzet,
      gva_not_change(1) TYPE c.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed   FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finish FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      hotspot_click    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          lv_value_dt  TYPE lvc_value,
          lv_value_hr  TYPE lvc_value,
          lt_zlest0061 TYPE TABLE OF zlest0061,
          ls_zlest0061 TYPE zlest0061,
          lt_zlest0060 TYPE TABLE OF zlest0060,
          ls_zlest0060 TYPE zlest0060.

    DATA: var_peso_chegada    TYPE zlest0061-peso_chegada,
          vl_tot_peso_chegada TYPE zlest0061-peso_chegada,
          vl_dif_peso_chegada TYPE zlest0061-peso_chegada.

    DATA: var_peso_chegada_nf    TYPE zlest0061-peso_chegada,
          vl_tot_peso_chegada_nf TYPE zlest0061-peso_chegada,
          vl_dif_peso_chegada_nf TYPE zlest0061-peso_chegada,
          vl_peso_vinc_nf        TYPE zlest0060-peso_fiscal.

    DATA:  lt_zlest0060_ck TYPE TABLE OF zlest0060.


****    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'DT_DESCARGA_INI' OR
****                                                              fieldname = 'HR_DESCARGA_INI' OR
****                                                              fieldname = 'DT_DESCARGA_FIM' OR
****                                                              fieldname = 'HR_DESCARGA_FIM'.
****
****      lv_value = ls_good-value.
****      CONDENSE lv_value NO-GAPS.
****
****      CHECK NOT ( lv_value IS INITIAL ).
****
****      READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.
****
****      CHECK sy-subrc EQ 0.
****
****      CASE ls_good-fieldname.
****        WHEN 'DT_DESCARGA_INI'.
****          gw_saida_viagem-dt_descarga_ini = lv_value.
****        WHEN 'HR_DESCARGA_INI'.
****          gw_saida_viagem-hr_descarga_ini = lv_value.
****        WHEN 'DT_DESCARGA_FIM'.
****          gw_saida_viagem-dt_descarga_fim = lv_value.
****        WHEN 'HR_DESCARGA_FIM'.
****          gw_saida_viagem-hr_descarga_fim = lv_value.
****      ENDCASE.
****
****      IF ( gw_saida_viagem-dt_descarga_fim IS NOT INITIAL ) AND ( gw_saida_viagem-dt_descarga_ini IS NOT INITIAL ).
****        IF ( gw_saida_viagem-dt_descarga_fim < gw_saida_viagem-dt_descarga_ini ).
****          MESSAGE s899(fi) DISPLAY LIKE 'W'  WITH 'Data de Descarga Final' 'menor que Data Descarga Inicial!'.
****          CONTINUE.
****        ELSEIF gw_saida_viagem-dt_descarga_fim EQ gw_saida_viagem-dt_descarga_ini.
****          IF gw_saida_viagem-hr_descarga_fim < gw_saida_viagem-hr_descarga_ini.
****            MESSAGE s899(fi) DISPLAY LIKE 'W'  WITH 'Hora de Descarga Final' 'menor que Hora Descarga Inicial!'.
****            CONTINUE.
****          ENDIF.
****        ENDIF.
****      ENDIF.
****
****      CLEAR: lt_zlest0061[], lt_zlest0060[].
*****     "// Verfica se o Material esta no SET caso esteja não considerar o material no select
****      PERFORM fm_checar_material USING gw_saida_viagem-cod_material.
****
****      SELECT * FROM zlest0061
****        INTO TABLE lt_zlest0061
****      WHERE bukrs        EQ gw_saida_viagem-bukrs
****        AND werks        EQ gw_saida_viagem-werks
****        AND ano_viagem   EQ gw_saida_viagem-ano_viagem
****        AND nr_viagem    EQ gw_saida_viagem-nr_viagem
****        AND nome_emb     EQ gw_saida_viagem-nome_emb
****        AND cod_material IN r_matnr.
****
****      CHECK ( lt_zlest0061[] IS NOT INITIAL ) .
****
****      SELECT *
****        FROM zlest0060 INTO TABLE lt_zlest0060
****         FOR ALL ENTRIES IN lt_zlest0061
****       WHERE bukrs         EQ lt_zlest0061-bukrs
****         AND werks         EQ lt_zlest0061-werks
****         AND nr_viagem     EQ lt_zlest0061-nr_viagem
****         AND ano_viagem    EQ lt_zlest0061-ano_viagem
****         AND embarcacao    EQ lt_zlest0061-embarcacao
****         AND nome_emb      EQ lt_zlest0061-nome_emb
****         AND cl_codigo     EQ lt_zlest0061-cl_codigo
****         AND nr_dco        EQ lt_zlest0061-nr_dco
****         AND safra         EQ lt_zlest0061-safra
****         AND docnum        EQ lt_zlest0061-docnum
****         AND id_frete_aqua EQ lt_zlest0061-id_frete_aqua.
****
****      LOOP AT lt_zlest0061 INTO ls_zlest0061.
****        UPDATE zlest0061
****           SET dt_descarga_ini  = gw_saida_viagem-dt_descarga_ini
****               hr_descarga_ini  = gw_saida_viagem-hr_descarga_ini
****               dt_descarga_fim  = gw_saida_viagem-dt_descarga_fim
****               hr_descarga_fim  = gw_saida_viagem-hr_descarga_fim
****         WHERE docnum EQ ls_zlest0061-docnum.
****
****        UPDATE zlest0060
****           SET dt_descarga_ini  = gw_saida_viagem-dt_descarga_ini
****               hr_descarga_ini  = gw_saida_viagem-hr_descarga_ini
****               dt_descarga_fim  = gw_saida_viagem-dt_descarga_fim
****               hr_descarga_fim  = gw_saida_viagem-hr_descarga_fim
****         WHERE docnum EQ ls_zlest0061-docnum.
****      ENDLOOP.
****
****      CALL METHOD obj_grid_viagem->refresh_table_display
****        EXPORTING
****          is_stable = wa_stable.
****
****    ENDLOOP.

***/// CS2021000269 Travas no lançamento de Barcaças ZLES0088 - Set 2021 - Inicio
******** Inserir uma validação que a Data de descarga Inicio e Data de descarga Fim,
******** não deve ultrapassar 365 dias da Data de Chegada.
********_____________________________________________________________________________

*3º - Permitir modificar as viagem das barcaça com as datas fechadas, se o usuário tiver no perfil o objeto de autorização que será criado.
*4º - Permitir que usuário faça alteração da data e hora inicio da chegada, se o usuário tiver no perfil o objeto de autorização que será criado.
*5º - Se o usuário tiver o acesso para alterar a data e hora da chegada, validar após o lançamento se a data e hora da descarga e maior que a data e hora da chegada que esta sendo modificada.
*6º - Validar a data e hora da descarga:  Se for menor que a data e hora inicio da chegada, informar ao usuário através mensagem na tela, ''Data inicio de descarga menor que data da chegada''.
*7º - Validar a hora da descarga quando a data da descarga for igual a data da chegada: Se a hora da data descarga for menor que hora da data chegada, informar o usuário com msg: "Hora da descarga e menor que hora da chegada".
*8º - Validar a data e hora fim da descarga, caso seja maior que a data e hora atual do sistema, informar ao usuário através mensagem: "Data de descarga fim maior que data atual".


    DATA: vl_datediff TYPE  p,
          vl_timediff TYPE  p,
          vl_earliest TYPE  c.

    FIELD-SYMBOLS: <fs_saida> TYPE ty_saida_viagem.

    DATA: vl_dt_inicio TYPE timestamp,
          vl_dt_fim    TYPE timestamp,
          vl_date      TYPE d,
          vl_hour      TYPE sy-uzeit.


    DATA: vl_status TYPE sy-subrc.

    CLEAR: vl_dt_inicio,
           vl_dt_fim.



    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'DT_DESCARGA_INI' OR
                                                              fieldname = 'HR_DESCARGA_INI' OR
                                                              fieldname = 'DT_DESCARGA_FIM' OR
                                                              fieldname = 'HR_DESCARGA_FIM' OR
                                                              fieldname = 'DATA_CHEGADA'    OR
                                                              fieldname = 'HR_PREVISTA'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      CHECK NOT ( lv_value IS INITIAL ).

      READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.

      IF sy-subrc = 0 AND ( ls_good-fieldname = 'DT_DESCARGA_INI'    OR ls_good-fieldname = 'DT_DESCARGA_FIM' ).

*Inserir uma validação que a Data de descarga Inicio
*e Data de descarga Fim, não deve ultrapassar 365 dias
*da Data de Chegada.
*_______________________________________________________
        CLEAR vl_status.
        PERFORM f_valida_365 USING lv_value ls_good-fieldname gw_saida_viagem vl_status.

        IF vl_status > 0.
          IF  ls_good-fieldname = 'DT_DESCARGA_INI'.
            PERFORM f_display_erro USING 'S'
                                         er_data_changed
                                         ls_good-fieldname
                                         ls_good-row_id
                                         'Diferença da Data'
                                         ' de descarga inicial'
                                         ' da chegada maior '
                                         'que 365 Dias'.
*            MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Diferença da Data' 'de descarga inicial' 'da chegada maior que 365 Dias'.
            CONTINUE.
          ENDIF.
          IF  ls_good-fieldname = 'DT_DESCARGA_FIM'.
            PERFORM f_display_erro USING 'S'
                                         er_data_changed
                                         ls_good-fieldname
                                         ls_good-row_id
                                         'Diferença da Data'
                                         ' de descarga final'
                                         ' da chegada maior '
                                         'que 365 Dias'.
*            MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Diferença da Data' 'de descarga final' 'da chegada maior que 365 Dias'.
            CONTINUE.
          ENDIF.

        ENDIF.
*** BUG - 72072 - Inicio
        IF  ls_good-fieldname = 'DT_DESCARGA_INI'.
          PERFORM f_valida_dt_ini_chegada USING lv_value ls_good-fieldname gw_saida_viagem vl_status.
          IF vl_status > 0.
            PERFORM f_display_erro USING 'S'
                                          er_data_changed
                                          ls_good-fieldname
                                          ls_good-row_id
                                          'Data/Hora da descarga Ini'
                                          ' é maior que data/hora '
                                          '  da descarga fim'
                                          ' '.
*          MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Data/Hora da descarga e menor que data/hora da chegada'.
            CONTINUE.
          ENDIF.

        ENDIF.


        IF  ls_good-fieldname = 'DT_DESCARGA_FIM'.
          PERFORM f_valida_dt_ini_chegada USING lv_value ls_good-fieldname gw_saida_viagem vl_status.
          IF vl_status > 0.
            PERFORM f_display_erro USING 'S'
                                          er_data_changed
                                          ls_good-fieldname
                                          ls_good-row_id
                                          'Data/Hora da descarga Fim'
                                          ' é menor que data/hora descarga Ini'
                                          ' Ou data descarga Ini está nula '
                                          ' '.
*          MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Data/Hora da descarga e menor que data/hora da chegada'.
            CONTINUE.
          ENDIF.
        ENDIF.
*** BUG - 72072 - Fim
      ENDIF.


* 5º - Se o usuário tiver o acesso para alterar a data e hora da chegada,
* validar após o lançamento se a data e hora da descarga
* e maior que a data e hora da chegada que esta sendo modificada.
*_____________________________________________________________________________

      IF sy-subrc = 0 AND ( ls_good-fieldname = 'DATA_CHEGADA'  OR ls_good-fieldname = 'HR_PREVISTA' ).
        CLEAR vl_status.

        PERFORM f_valida_dt_ini_chegada USING lv_value ls_good-fieldname gw_saida_viagem vl_status.
        IF vl_status > 0.
          PERFORM f_display_erro USING 'S'
                                        er_data_changed
                                        ls_good-fieldname
                                        ls_good-row_id
                                        'Data/Hora da descarga'
                                        ' e menor que data/hora '
                                        '  da chegada'
                                        ' '.
*          MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Data/Hora da descarga e menor que data/hora da chegada'.
          CONTINUE.
        ENDIF.
* valida nota cct
*--------------------------------------------------------
        IF ls_good-fieldname = 'DATA_CHEGADA' .
***///
          CLEAR: nfnum.
          PERFORM f_check_cct USING ls_good-row_id CHANGING nfnum.
          IF nfnum IS NOT INITIAL.
            PERFORM f_display_erro USING 'N'
                                        er_data_changed
                                        ls_good-fieldname
                                        ls_good-row_id
                                        ' Nota '
                                        nfnum
                                        ' vinculada a essa barcaça '
                                        ' já enviada ao CCT .'.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'DATA_CHEGADA'
                i_value     = gw_saida_viagem-data_chegada.
            CONTINUE.
          ENDIF.
        ENDIF.
***///
        "BUG IMPEDITIVO 76866 quando alterar hora chegada validar vinculação cct
        IF ls_good-fieldname = 'HR_PREVISTA' .
***///
          CLEAR: nfnum.
          PERFORM f_check_cct USING ls_good-row_id CHANGING nfnum.
          IF nfnum IS NOT INITIAL.
            PERFORM f_display_erro USING 'N'
                                        er_data_changed
                                        ls_good-fieldname
                                        ls_good-row_id
                                        ' Nota '
                                        nfnum
                                        ' vinculada a essa barcaça '
                                        ' já enviada ao CCT .'.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'HR_PREVISTA'
                i_value     = gw_saida_viagem-hr_prevista.
            CONTINUE.
          ENDIF.
        ENDIF.
        "<<< END
      ENDIF.


* 6º - Validar a data e hora da descarga:
*Se for menor que a data e hora inicio da chegada,
* informar ao usuário através mensagem na tela,
*''Data inicio de descarga menor que data da chegada''.
*_____________________________________________________________________________
      IF sy-subrc = 0 AND ( ls_good-fieldname = 'DT_DESCARGA_INI' OR ls_good-fieldname = 'HR_DESCARGA_INI' ).
        CLEAR vl_status.
        PERFORM f_valida_dt_inicial USING lv_value ls_good-fieldname gw_saida_viagem vl_status.
        IF vl_status > 0.
          PERFORM f_display_erro USING 'S'
                                        er_data_changed
                                        ls_good-fieldname
                                        ls_good-row_id
                                        'Data inicio de descarga'
                                        ' menor que data da chegada'
                                         ' '
                                         ' '.
*          MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Data inicio de descarga menor que data da chegada'.
          CONTINUE.
        ENDIF.
      ENDIF.


* 7º - Validar a hora da descarga quando a data da descarga for igual a data da chegada:
* Se a hora da data descarga for menor que hora da data chegada,
* informar o usuário com msg: "Hora da descarga e menor que hora da chegada".
*_____________________________________________________________________________
      IF ls_good-fieldname = 'DT_DESCARGA_INI' OR ls_good-fieldname = 'DATA_CHEGADA'.
        IF ( ls_good-fieldname = 'DT_DESCARGA_INI' AND ( gw_saida_viagem-data_chegada = lv_value ) ) OR
           ( ls_good-fieldname = 'DATA_CHEGADA'    AND ( gw_saida_viagem-dt_descarga_ini = lv_value ) ).
          PERFORM f_valida_dt_descarga USING lv_value ls_good-fieldname gw_saida_viagem vl_status.
          IF vl_status > 0.
            PERFORM f_display_erro USING 'S'
                                        er_data_changed
                                        ls_good-fieldname
                                        ls_good-row_id
                                        'Hora da descarga e'
                                        ' menor que hora'
                                        '  da chegada'
                                        ' '.
*            MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Hora da descarga e menor que hora da chegada'.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

* 8º - Validar a data e hora fim da descarga,
*caso seja maior que a data e hora atual do sistema,
*informar ao usuário através mensagem: "Data de descarga
*<fim maior que data atual".
*_____________________________________________________________________________
      "   IF sy-subrc = 0 AND ( ls_good-fieldname = 'DT_DESCARGA_FIM' OR ls_good-fieldname = 'HR_DESCARGA_FIM' ). "BUG IMPEDITIVO 76343
      IF sy-subrc = 0 AND ( ls_good-fieldname = 'DT_DESCARGA_FIM' ) .
        CLEAR vl_status.
        PERFORM f_valida_dt_sistema USING lv_value ls_good-fieldname gw_saida_viagem vl_status.
        IF vl_status > 0.
          PERFORM f_display_erro USING 'S'
                                        er_data_changed
                                        ls_good-fieldname
                                        ls_good-row_id
                                        'Data de descarga fim maior'
                                        ' que data atual ou '
                                        ' Dt Descarga fim menor'
                                        ' que inicial'.
*          MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Data de descarga fim maior' 'que data atual ou ' 'Dt Descarga fim menor' 'que inicial'.
          CONTINUE.
        ENDIF.
      ENDIF.
      "BUG IMPEDITIVO 76343 Somente desmebrado para apresentar mensagem da Hora
      IF sy-subrc = 0 AND ( ls_good-fieldname = 'HR_DESCARGA_FIM' ) .
        CLEAR vl_status.
        PERFORM f_valida_dt_sistema USING lv_value ls_good-fieldname gw_saida_viagem vl_status.
        IF vl_status > 0.
          PERFORM f_display_erro USING 'S'
                                        er_data_changed
                                        ls_good-fieldname
                                        ls_good-row_id
                                        'Hora de descarga fim '
                                        ' menor que hora   '
                                        ' de descarga'
                                        ' inicial '.
*          MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'Data de descarga fim maior' 'que data atual ou ' 'Dt Descarga fim menor' 'que inicial'.
          CONTINUE.
        ENDIF.
      ENDIF.


* recho migrado do codigo acima
*_____________________________________________________________________________
      IF ls_good-fieldname = 'DT_DESCARGA_INI' OR
         ls_good-fieldname = 'HR_DESCARGA_INI' OR
         ls_good-fieldname = 'DT_DESCARGA_FIM' OR
         ls_good-fieldname = 'HR_DESCARGA_FIM' OR
         ls_good-fieldname = 'DATA_CHEGADA'    OR  "BUG - 72072 -  CBRAND
         ls_good-fieldname = 'HR_PREVISTA'. "BUG - 72072 -  CBRAND

        lv_value = ls_good-value.
        CONDENSE lv_value NO-GAPS.

        CHECK NOT ( lv_value IS INITIAL ).

        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.

        CHECK sy-subrc EQ 0.

        CASE ls_good-fieldname.
          WHEN 'DT_DESCARGA_INI'.
            gw_saida_viagem-dt_descarga_ini = lv_value.
          WHEN 'HR_DESCARGA_INI'.
            gw_saida_viagem-hr_descarga_ini = lv_value.
          WHEN 'DT_DESCARGA_FIM'.
            gw_saida_viagem-dt_descarga_fim = lv_value.
          WHEN 'HR_DESCARGA_FIM'.
            gw_saida_viagem-hr_descarga_fim = lv_value.
          WHEN 'DATA_CHEGADA'.
            gw_saida_viagem-data_chegada = lv_value. "BUG - 72072 - CBRAND
          WHEN 'HR_PREVISTA'.
            gw_saida_viagem-hr_prevista = lv_value. "BUG - 72072 - CBRAND
        ENDCASE.


        CLEAR: lt_zlest0061[], lt_zlest0060[].
*     "// Verfica se o Material esta no SET caso esteja não considerar o material no select
        PERFORM fm_checar_material USING gw_saida_viagem-cod_material.

        SELECT * FROM zlest0061
          INTO TABLE lt_zlest0061
        WHERE bukrs        EQ gw_saida_viagem-bukrs
          AND werks        EQ gw_saida_viagem-werks
          AND ano_viagem   EQ gw_saida_viagem-ano_viagem
          AND nr_viagem    EQ gw_saida_viagem-nr_viagem
          AND nome_emb     EQ gw_saida_viagem-nome_emb
          AND cod_material IN r_matnr.

        CHECK ( lt_zlest0061[] IS NOT INITIAL ) .

        SELECT *
          FROM zlest0060 INTO TABLE lt_zlest0060
           FOR ALL ENTRIES IN lt_zlest0061
         WHERE bukrs         EQ lt_zlest0061-bukrs
           AND werks         EQ lt_zlest0061-werks
           AND nr_viagem     EQ lt_zlest0061-nr_viagem
           AND ano_viagem    EQ lt_zlest0061-ano_viagem
           AND embarcacao    EQ lt_zlest0061-embarcacao
           AND nome_emb      EQ lt_zlest0061-nome_emb
           AND cl_codigo     EQ lt_zlest0061-cl_codigo
           AND nr_dco        EQ lt_zlest0061-nr_dco
           AND safra         EQ lt_zlest0061-safra
           AND docnum        EQ lt_zlest0061-docnum
           AND id_frete_aqua EQ lt_zlest0061-id_frete_aqua.

        LOOP AT lt_zlest0061 INTO ls_zlest0061.
          UPDATE zlest0061
             SET dt_descarga_ini  = gw_saida_viagem-dt_descarga_ini
                 hr_descarga_ini  = gw_saida_viagem-hr_descarga_ini
                 dt_descarga_fim  = gw_saida_viagem-dt_descarga_fim
                 hr_descarga_fim  = gw_saida_viagem-hr_descarga_fim
                 dt_chegada       = gw_saida_viagem-data_chegada  "BUG - 72072 - CBRAND
                 hr_chegada       = gw_saida_viagem-hr_prevista   "BUG - 72072 - CBRAND
           WHERE docnum EQ ls_zlest0061-docnum.

          UPDATE zlest0060
             SET dt_descarga_ini  = gw_saida_viagem-dt_descarga_ini
                 hr_descarga_ini  = gw_saida_viagem-hr_descarga_ini
                 dt_descarga_fim  = gw_saida_viagem-dt_descarga_fim
                 hr_descarga_fim  = gw_saida_viagem-hr_descarga_fim
                 dt_chegada       = gw_saida_viagem-data_chegada "BUG - 72072 - CBRAND
                 hr_chegada       = gw_saida_viagem-hr_prevista  "BUG - 72072 - CBRAND
           WHERE docnum EQ ls_zlest0061-docnum.
        ENDLOOP.

        CALL METHOD obj_grid_viagem->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      ENDIF.

    ENDLOOP.
***///

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'PESO_DESCARGA'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      IF NOT ( lv_value IS INITIAL ).

        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.

***/// Não precisava fazer trava do peso de chegada, pois o peso que vai para o CCT é o peso fiscal
*        CLEAR nfnum.
*        PERFORM f_check_cct USING ls_good-row_id CHANGING nfnum.
*
*        IF nfnum IS NOT INITIAL.
*          PERFORM f_display_erro USING 'N'
*                                       er_data_changed
*                                       ls_good-fieldname
*                                       ls_good-row_id
*                                       'NF-'
*                                       nfnum
*                                       ' vinculada a essa barcaça '
*                                       ' já enviada ao CCT .'.
*          CALL METHOD er_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'PESO_DESCARGA'
*              i_value     = gw_saida_viagem-peso_descarga.
*          EXIT.
*        ENDIF.
***///

        IF ( gw_saida_viagem-data_chegada IS INITIAL ).
          MESSAGE s899(fi) DISPLAY LIKE 'W'  WITH 'Informar a Data de Chegada'.
          CONTINUE.
        ENDIF.

        IF ( gw_saida_viagem-ck_terminal IS INITIAL ).
          gw_saida_viagem-ck_terminal = 'X'.
          MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX ls_good-row_id TRANSPORTING ck_terminal.
        ENDIF.

        IF NOT ( gw_saida_viagem-estilo IS INITIAL ).
          CLEAR: gw_saida_viagem-estilo.
          MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX ls_good-row_id TRANSPORTING estilo.
        ENDIF.

        REFRESH: gt_estilo[].
        CLEAR: gw_estilo.

        "Calculo para salvar na ZLEST0061
        SELECT * FROM zlest0061
          INTO TABLE lt_zlest0061
        WHERE bukrs        EQ gw_saida_viagem-bukrs
          AND werks        EQ gw_saida_viagem-werks
          AND ano_viagem   EQ gw_saida_viagem-ano_viagem
          AND nr_viagem    EQ gw_saida_viagem-nr_viagem
          AND nome_emb     EQ gw_saida_viagem-nome_emb.
*          AND cod_material IN r_matnr. //" Removido pq temos empresas com materiais diferentes.

        CHECK NOT ( lt_zlest0061 IS INITIAL ) .
        CLEAR: lt_zlest0060[].
        SELECT *
          FROM zlest0060 INTO TABLE lt_zlest0060
           FOR ALL ENTRIES IN lt_zlest0061
         WHERE bukrs       EQ lt_zlest0061-bukrs
           AND werks       EQ lt_zlest0061-werks
           AND nr_viagem   EQ lt_zlest0061-nr_viagem
           AND ano_viagem  EQ lt_zlest0061-ano_viagem
           AND embarcacao  EQ lt_zlest0061-embarcacao
           AND nome_emb    EQ lt_zlest0061-nome_emb
           AND cl_codigo   EQ lt_zlest0061-cl_codigo
           AND nr_dco      EQ lt_zlest0061-nr_dco
           AND safra       EQ lt_zlest0061-safra
           AND docnum      EQ lt_zlest0061-docnum.

*        LOOP AT LT_ZLEST0060 INTO LS_ZLEST0060 WHERE ( ST_CCT EQ '01' ) OR  "Já disponibilizada para registro de CCT.
*                                                     ( ST_CCT EQ '02' ).    "Já Vinculado CCT.
*          IF SY-SUBRC = 0.
*            LV_VALUE = GW_SAIDA_VIAGEM-PESO_DESCARGA.
*
*            CONDENSE LV_VALUE NO-GAPS.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_FIELDNAME = 'PESO_DESCARGA'
*                I_VALUE     = LV_VALUE.
*
*            MESSAGE 'Já existem notas dessa Barcaça, que já foram disponibilizadas para o registro de CCT!' TYPE 'S'.
*            RETURN.
*          ENDIF.
*        ENDLOOP.

        IF  ( lt_zlest0061[] IS NOT INITIAL ).

          CLEAR: vl_tot_peso_chegada, vl_dif_peso_chegada.
          CLEAR: vl_tot_peso_chegada_nf, vl_dif_peso_chegada_nf.

          LOOP AT lt_zlest0061 INTO ls_zlest0061.

            CLEAR: var_peso_chegada.

            IF NOT ( ls_zlest0061-docnum IS INITIAL ).

              var_peso_chegada = ( ( lv_value / gw_saida_viagem-peso_faturado ) *  ls_zlest0061-peso_vinculado ).

              UPDATE zlest0061
                 SET peso_chegada        = var_peso_chegada
                     dt_chegada          = gw_saida_viagem-data_chegada
                     hr_chegada          = gw_saida_viagem-hr_prevista
                     dt_chegada_terminal = sy-datum
                WHERE docnum EQ ls_zlest0061-docnum.

              ADD var_peso_chegada TO vl_tot_peso_chegada.

              "Calcular Por Nota
              LOOP AT lt_zlest0060 INTO ls_zlest0060 WHERE docnum = ls_zlest0061-docnum.
                CLEAR: var_peso_chegada_nf, vl_peso_vinc_nf.

                IF ls_zlest0060-peso_liq_ret IS NOT INITIAL.
                  vl_peso_vinc_nf = ls_zlest0060-peso_liq_ret.
                ELSE.
                  vl_peso_vinc_nf = ls_zlest0060-peso_fiscal.
                ENDIF.

                var_peso_chegada_nf = ( ( lv_value / gw_saida_viagem-peso_faturado ) *  vl_peso_vinc_nf ).

                UPDATE zlest0060
                   SET peso_chegada = var_peso_chegada_nf
                       dt_chegada   = gw_saida_viagem-data_chegada
                       hr_chegada   = gw_saida_viagem-hr_prevista
                 WHERE bukrs         EQ ls_zlest0060-bukrs
                   AND werks         EQ ls_zlest0060-werks
                   AND nr_viagem     EQ ls_zlest0060-nr_viagem
                   AND ano_viagem    EQ ls_zlest0060-ano_viagem
                   AND embarcacao    EQ ls_zlest0060-embarcacao
                   AND nome_emb      EQ ls_zlest0060-nome_emb
                   AND rm_codigo     EQ ls_zlest0060-rm_codigo
                   AND dt_codigo     EQ ls_zlest0060-dt_codigo
                   AND nr_romaneio   EQ ls_zlest0060-nr_romaneio
                   AND safra         EQ ls_zlest0060-safra
                   AND chave_nfe     EQ ls_zlest0060-chave_nfe.

                ADD var_peso_chegada_nf TO vl_tot_peso_chegada_nf.
              ENDLOOP.

            ENDIF.

            CLEAR: var_peso_chegada, ls_zlest0061.
          ENDLOOP.

          IF ( vl_tot_peso_chegada > 0 ).

            vl_dif_peso_chegada =  lv_value - vl_tot_peso_chegada.

            IF ( vl_dif_peso_chegada NE 0 ).

              READ TABLE lt_zlest0061 INTO ls_zlest0061 INDEX 1.

              UPDATE zlest0061
                 SET peso_chegada = peso_chegada + vl_dif_peso_chegada
                WHERE docnum EQ ls_zlest0061-docnum.
            ENDIF.

            "NF
            vl_dif_peso_chegada_nf =  lv_value - vl_tot_peso_chegada_nf.

            IF ( vl_dif_peso_chegada_nf NE 0 ).

              READ TABLE lt_zlest0060 INTO ls_zlest0060 INDEX 1.

              UPDATE zlest0060
                 SET peso_chegada = peso_chegada + vl_dif_peso_chegada_nf
               WHERE bukrs         EQ ls_zlest0060-bukrs
                 AND werks         EQ ls_zlest0060-werks
                 AND nr_viagem     EQ ls_zlest0060-nr_viagem
                 AND ano_viagem    EQ ls_zlest0060-ano_viagem
                 AND embarcacao    EQ ls_zlest0060-embarcacao
                 AND nome_emb      EQ ls_zlest0060-nome_emb
                 AND rm_codigo     EQ ls_zlest0060-rm_codigo
                 AND dt_codigo     EQ ls_zlest0060-dt_codigo
                 AND nr_romaneio   EQ ls_zlest0060-nr_romaneio
                 AND safra         EQ ls_zlest0060-safra
                 AND chave_nfe     EQ ls_zlest0060-chave_nfe.
            ENDIF.

          ENDIF.


*          GW_ESTILO-FIELDNAME = 'DATA_CHEGADA'.
*          GW_ESTILO-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*          APPEND GW_ESTILO TO GT_ESTILO.
*
*          GW_ESTILO-FIELDNAME = 'HR_PREVISTA'.
*          GW_ESTILO-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*          APPEND GW_ESTILO TO GT_ESTILO.
*
*          GW_ESTILO-FIELDNAME = 'PESO_DESCARGA'.
*          GW_ESTILO-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*          APPEND GW_ESTILO TO GT_ESTILO.
*
*
*          INSERT LINES OF GT_ESTILO INTO TABLE GW_SAIDA_VIAGEM-ESTILO.
*          MODIFY GT_SAIDA_VIAGEM FROM GW_SAIDA_VIAGEM INDEX LS_GOOD-ROW_ID.
*
          "          WA_STABLE-ROW = 'X'.
          "         WA_STABLE-COL = 'X'.

          COMMIT WORK.

          CALL METHOD obj_grid_viagem->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.

      ENDIF.
    ENDLOOP.


    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'CK_TERMINAL'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      IF ( lv_value IS INITIAL ).

        "*** BUG - 70627  Inicio - CSB
        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.
        "*** BUG - 70627  Inicio - Fim

        CLEAR: nfnum.
        PERFORM f_check_cct USING ls_good-row_id CHANGING nfnum.
        IF nfnum IS NOT INITIAL.
          PERFORM f_display_erro USING 'N'
                                      er_data_changed
                                      ls_good-fieldname
                                      ls_good-row_id
                                      'NF-'
                                      nfnum
                                      '-vinculada a barcaça'
                                      '  já enviada CCT .'.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'CK_TERMINAL'
              i_value     = 'X'.
          EXIT.
*** BUG - 70627 - CSB - Inicio
        ELSE.
          PERFORM f_limpa_campos USING 'S' 'S' ls_good-row_id.
          IF gva_not_change = 'N'.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'CK_TERMINAL'
                i_value     = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
*** BUG - 70627 - CSB - Fim
***///
      ENDIF.

      IF NOT ( lv_value IS INITIAL ). " Desmarcou o flag na tela

        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.
***///
        CLEAR: nfnum.
        PERFORM f_check_cct USING ls_good-row_id CHANGING nfnum.

        IF nfnum IS NOT INITIAL.
          PERFORM f_display_erro USING 'N'
                                       er_data_changed
                                       ls_good-fieldname
                                       ls_good-row_id
                                       'NF-'
                                       nfnum
                                       ' vinculada a essa barcaça '
                                       ' já enviada ao CCT .'.
          EXIT.
*** BUG - 70627 - CSB - Inicio
*        ELSE.
*          PERFORM f_limpa_campos USING 'S' 'S' ls_good-row_id.
*** BUG - 70627 - CSB - Fim
        ENDIF.
***///
*     "// Verfica se o Material esta no SET caso esteja não considerar o material no select
        PERFORM fm_checar_material USING gw_saida_viagem-cod_material.

        SELECT *
          FROM zlest0061
          INTO TABLE @DATA(tl_zlest0061)
          WHERE bukrs      EQ @gw_saida_viagem-bukrs
          AND werks        EQ @gw_saida_viagem-werks
          AND ano_viagem   EQ @gw_saida_viagem-ano_viagem
          AND nr_viagem    EQ @gw_saida_viagem-nr_viagem
          AND nome_emb     EQ @gw_saida_viagem-nome_emb
          AND cod_material IN @r_matnr.

        IF lines( tl_zlest0061 ) > 0.
          LOOP AT tl_zlest0061 INTO DATA(wl_zlest0061).

            wl_zlest0061-dt_chegada_terminal = sy-datum.

            UPDATE zlest0061
              SET dt_chegada_terminal = wl_zlest0061-dt_chegada_terminal
              WHERE docnum EQ wl_zlest0061-docnum.

            COMMIT WORK.

            lv_value_dt = sy-datum.
            lv_value_hr = sy-uzeit.

            UPDATE zlest0061
               SET dt_chegada   = lv_value_dt
                   hr_chegada   = lv_value_hr
             WHERE docnum EQ wl_zlest0061-docnum.

            UPDATE zlest0060
               SET dt_chegada   = lv_value_dt
                   hr_chegada   = lv_value_hr
             WHERE docnum EQ wl_zlest0061-docnum.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'DATA_CHEGADA'
                i_value     = lv_value_dt.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'HR_PREVISTA'
                i_value     = lv_value_hr.
***///
***            CALL METHOD er_data_changed->modify_cell
***              EXPORTING
***                i_row_id    = ls_good-row_id
***                i_fieldname = 'DT_DESCARGA_INI'
***                i_value     = ''.
***
***            CALL METHOD er_data_changed->modify_cell
***              EXPORTING
***                i_row_id    = ls_good-row_id
***                i_fieldname = 'HR_DESCARGA_INI'
***                i_value     = ''.
***
***            CALL METHOD er_data_changed->modify_cell
***              EXPORTING
***                i_row_id    = ls_good-row_id
***                i_fieldname = 'DT_DESCARGA_FIM'
***                i_value     = ''.
***
***            CALL METHOD er_data_changed->modify_cell
***              EXPORTING
***                i_row_id    = ls_good-row_id
***                i_fieldname = 'HR_DESCARGA_FIM'
***                i_value     = ''.

***///
            CALL METHOD obj_grid_viagem->refresh_table_display
              EXPORTING
                is_stable = wa_stable.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED_OV

  METHOD on_data_changed_finish.
    DATA: ls_good         TYPE lvc_s_modi,
          lv_value        TYPE lvc_value,
          lt_zlest0061    TYPE TABLE OF zlest0061,
          ls_zlest0061    TYPE zlest0061,
          lt_zlest0060    TYPE TABLE OF zlest0060,
          lt_zlest0060_ck TYPE TABLE OF zlest0060,
          ls_zlest0060    TYPE zlest0060.

    DATA: vl_aut TYPE sy-subrc.

    "Check de permissão de edição
    CLEAR: vl_aut.
    PERFORM check_autorizacao CHANGING vl_aut.

    LOOP AT et_good_cells INTO ls_good WHERE fieldname = 'CK_TERMINAL'.

      lv_value = ls_good-value.

      READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.

*** BUG - 7627 - Inicio - CSB ( Novos Ajustes)
      IF ( gw_saida_viagem-ck_terminal IS INITIAL ).

        LOOP AT lt_zlest0060_ck INTO DATA(wl_zlest0060_ck) WHERE ( docnum_rem IS NOT INITIAL ) AND ( operacao EQ 'RI').
          SELECT SINGLE *
            FROM zfiwrt0008 INTO @DATA(wl_0008_ent)
           WHERE docnum_saida    EQ @wl_zlest0060_ck-docnum_rem
             AND docs_estornados EQ @abap_false
             AND loekz           EQ @abap_false.

          IF sy-subrc EQ 0.
            gw_saida_viagem-ck_terminal = 'X'.
            MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX ls_good-row_id TRANSPORTING ck_terminal.
            COMMIT WORK.
            CALL METHOD obj_grid_viagem->refresh_table_display
              EXPORTING
                is_stable = wa_stable.
            MESSAGE 'Já realizado entrada de Industrialização. Impossível desmarcar' TYPE 'I'.
            EXIT.
          ENDIF.
        ENDLOOP.


        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.
        IF vl_aut = 0.
          CLEAR: gw_saida_viagem-peso_descarga.
        ENDIF.
        IF ( gw_saida_viagem-peso_descarga IS NOT INITIAL ).
          gw_saida_viagem-ck_terminal = 'X'.
          MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX ls_good-row_id TRANSPORTING ck_terminal.
          COMMIT WORK.
          CALL METHOD obj_grid_viagem->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
          MESSAGE 'Informações de Recebimento preenchidas. Impossível desmarcar' TYPE 'I'.
          EXIT.

        ELSE.

          SELECT *
          FROM zlest0061
          INTO TABLE @DATA(tl_zlest0061)
          WHERE bukrs      EQ @gw_saida_viagem-bukrs
          AND werks        EQ @gw_saida_viagem-werks
          AND ano_viagem   EQ @gw_saida_viagem-ano_viagem
          AND nr_viagem    EQ @gw_saida_viagem-nr_viagem
          AND nome_emb     EQ @gw_saida_viagem-nome_emb
          AND cod_material EQ @gw_saida_viagem-cod_material.

          IF ( sy-subrc EQ 0 ).
            LOOP AT tl_zlest0061 INTO DATA(wl_zlest0061).

              UPDATE zlest0061
                SET dt_chegada_terminal = ''
                WHERE docnum EQ wl_zlest0061-docnum.

              UPDATE zlest0061
                 SET dt_chegada   = '00000000'
                     hr_chegada   = '000000'
               WHERE docnum EQ wl_zlest0061-docnum.

              UPDATE zlest0060
                 SET dt_chegada   = '00000000'
                     hr_chegada   = '000000'
               WHERE docnum EQ wl_zlest0061-docnum.

              COMMIT WORK.

            ENDLOOP.

          ENDIF.

          CALL METHOD obj_grid_viagem->refresh_table_display
            EXPORTING
              is_stable = wa_stable.


        ENDIF.

      ENDIF.

** BUG - 7627 - Fim - CSB ( Novos Ajustes)

*** BUG - 7627 - Inicio - CSb - Código Original.
*      IF ( lv_value IS INITIAL ).
*
*        LOOP AT lt_zlest0060_ck INTO DATA(wl_zlest0060_ck) WHERE ( docnum_rem IS NOT INITIAL ) AND ( operacao EQ 'RI').
*          SELECT SINGLE *
*            FROM zfiwrt0008 INTO @DATA(wl_0008_ent)
*           WHERE docnum_saida    EQ @wl_zlest0060_ck-docnum_rem
*             AND docs_estornados EQ @abap_false
*             AND loekz           EQ @abap_false.
*
*          IF sy-subrc EQ 0.
*            gw_saida_viagem-ck_terminal = 'X'.
*            MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX ls_good-row_id TRANSPORTING ck_terminal.
*            COMMIT WORK.
*            CALL METHOD obj_grid_viagem->refresh_table_display
*              EXPORTING
*                is_stable = wa_stable.
*            MESSAGE 'Já realizado entrada de Industrialização. Impossível desmarcar' TYPE 'I'.
*            EXIT.
*          ENDIF.
*        ENDLOOP.
*
*
*        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.
*        IF vl_aut = 0.
*          CLEAR: gw_saida_viagem-peso_descarga.
*        ENDIF.
*        IF ( gw_saida_viagem-peso_descarga IS NOT INITIAL ).
*          gw_saida_viagem-ck_terminal = 'X'.
*          MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX ls_good-row_id TRANSPORTING ck_terminal.
*          COMMIT WORK.
*          CALL METHOD obj_grid_viagem->refresh_table_display
*            EXPORTING
*              is_stable = wa_stable.
*          MESSAGE 'Informações de Recebimento preenchidas. Impossível desmarcar' TYPE 'I'.
*          EXIT.
*        ELSE.
*
*          SELECT *
*          FROM zlest0061
*          INTO TABLE @DATA(tl_zlest0061)
*          WHERE bukrs      EQ @gw_saida_viagem-bukrs
*          AND werks        EQ @gw_saida_viagem-werks
*          AND ano_viagem   EQ @gw_saida_viagem-ano_viagem
*          AND nr_viagem    EQ @gw_saida_viagem-nr_viagem
*          AND nome_emb     EQ @gw_saida_viagem-nome_emb
*          AND cod_material EQ @gw_saida_viagem-cod_material.
*
*          IF ( sy-subrc EQ 0 ).
*            LOOP AT tl_zlest0061 INTO DATA(wl_zlest0061).
*
*              UPDATE zlest0061
*                SET dt_chegada_terminal = ''
*                WHERE docnum EQ wl_zlest0061-docnum.
*
*              UPDATE zlest0061
*                 SET dt_chegada   = '00000000'
*                     hr_chegada   = '000000'
*               WHERE docnum EQ wl_zlest0061-docnum.
*
*              UPDATE zlest0060
*                 SET dt_chegada   = '00000000'
*                     hr_chegada   = '000000'
*               WHERE docnum EQ wl_zlest0061-docnum.
*
*              COMMIT WORK.
*
*            ENDLOOP.
*
*          ENDIF.
*
*          CALL METHOD obj_grid_viagem->refresh_table_display
*            EXPORTING
*              is_stable = wa_stable.
*
*
*        ENDIF.
*
*      ENDIF.
*** BUG - 7627 - Fim - CSb - Código Original.

    ENDLOOP.

    CALL METHOD obj_grid_viagem->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD hotspot_click.

    CASE e_column_id.

      WHEN: 'EDIT'.

        REFRESH: gt_estilo[].
        CLEAR: gw_estilo.

        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX e_row_id.

        IF NOT ( gw_saida_viagem-estilo IS INITIAL ).
          CLEAR: gw_saida_viagem-estilo.
          MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX e_row_id TRANSPORTING estilo.
        ENDIF.

        gw_estilo-fieldname = 'CK_TERMINAL'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND gw_estilo TO gt_estilo.


        gw_estilo-fieldname = 'DATA_CHEGADA'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND gw_estilo TO gt_estilo.


        gw_estilo-fieldname = 'DT_DESCARGA_FIM'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'DT_DESCARGA_INI'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'HR_DESCARGA_FIM'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'HR_DESCARGA_INI'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'HR_PREVISTA'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND gw_estilo TO gt_estilo.


        gw_estilo-fieldname = 'PESO_DESCARGA'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND gw_estilo TO gt_estilo.


        INSERT LINES OF gt_estilo INTO TABLE gw_saida_viagem-estilo.
        MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX e_row_id.


        wa_stable-row = 'X'.
        wa_stable-col = 'X'.

        CALL METHOD obj_grid_viagem->refresh_table_display
          EXPORTING
            is_stable = wa_stable.


      WHEN: 'VISU'.

        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX e_row_id.
        IF ( sy-subrc EQ 0 ).
          "PERFORM: ABRIR_CTE_NOTAS USING GW_SAIDA_VIAGEM.
          PERFORM: selecionar_cte USING gw_saida_viagem.
          CLEAR: var_title_popup.
          var_title_popup = gw_saida_viagem-nome_emb.

          CALL SCREEN 0400 STARTING AT 10 1 ENDING AT 100 20.
        ENDIF.

      WHEN 'SEQ_LCTO_IND'.

        READ TABLE gt_saida_nfe INTO gw_saida_nfe INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( gw_saida_nfe-seq_lcto_ind IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  gw_saida_nfe-seq_lcto_ind.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_IND'.

        READ TABLE gt_saida_nfe INTO gw_saida_nfe INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( gw_saida_nfe-docnum_ind IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD gw_saida_nfe-docnum_ind.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_IND'.

        READ TABLE gt_saida_nfe INTO gw_saida_nfe INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( gw_saida_nfe-seq_lcto_ind IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            gw_saida_nfe-seq_lcto_ind,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.



    ENDCASE.

  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

ENDCLASS.                    "LCL_EVENT IMPLEMENTATION


CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_generate.
    ty_toolbar-function  = 'GER_ENT_IND'.
    ty_toolbar-text      = 'Gerar Entrada Ind.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*** Inicio - Rubenilson Pereira - 16.06.2022 - 54384

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'ZLES0147'
      EXCEPTIONS
        ok     = 1
        not_ok = 2.
    IF sy-subrc  EQ 1.

      ty_toolbar-icon      = icon_generate.
      ty_toolbar-function  = 'REC_CARGA_CCT'.
      ty_toolbar-text      = 'Recepcionar Carga - CCT.'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

    ENDIF.
*** Fim - Rubenilson Pereira - 16.06.2022 - 54384

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'GER_ENT_IND'.
        PERFORM f_lcto_ent_ind_znfw.
*** Inicio - Rubenilson Pereira - 16.06.2022 - #54384
      WHEN 'REC_CARGA_CCT'.

        PERFORM f_rec_carga_cct USING e_ucomm.
*** Fim - Rubenilson Pereira - 16.06.2022 - #54384

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

START-OF-SELECTION.
*&--------------------------------------------------------------------&*
*& Iniciar Tela
*&--------------------------------------------------------------------&*
  CALL SCREEN tela_0100.
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'PF0100'. "Status da Tela 0100.
  SET TITLEBAR  'TB0100'. "Title da Tela 0100
  PERFORM: criar_alv_chegada.
ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  IF wp_tela_0100-wp_cte = 'X'.
    var_tp_alv = '2'.
  ELSEIF wp_tela_0100-wp_barcaca = 'X'.
    var_tp_alv = '1'.
  ELSEIF wp_tela_0100-wp_nfe = 'X'.
    var_tp_alv = '3'.
  ENDIF.

  CASE ok_code.
    WHEN: 'BACK'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'. "Botão para sair do programa corrente.
      LEAVE PROGRAM.
    WHEN: 'BTN_BUSCA'.
      PERFORM: selecionar_nr_viagem.
    WHEN 'BTN_ENV_CCT'.
      PERFORM: disp_cct USING ok_code.
    WHEN 'BTN_REMOVER_CCT'.
      PERFORM: remover_cct.
    WHEN 'INF_DT_SAI_COMBOIO'.
      PERFORM: f_inf_dt_sai_comboio.
    WHEN: 'TIPO_ALV' OR 'ENTER'.
      PERFORM: preencher_campos.
      PERFORM: seleciona_dados_chegada.
      LEAVE TO SCREEN 0100.
  ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_CHEGADA
*&---------------------------------------------------------------------*
FORM seleciona_dados_chegada.

  DATA: lt_zlest0063 TYPE TABLE OF zlest0063,
        ls_zlest0063 TYPE zlest0063,

        lt_zlest0060 TYPE TABLE OF zlest0060,
        ls_zlest0060 TYPE zlest0060,

        lt_zlest0061 TYPE TABLE OF zlest0061,
        ls_zlest0061 TYPE zlest0061,

        lt_makt      TYPE TABLE OF makt,
        ls_makt      TYPE makt.

  DATA: var_total     TYPE zlest0060-peso_fiscal,
        var_total_rat TYPE zlest0060-peso_fiscal,
        var_data      TYPE zlest0061-dt_chegada,
        var_hora      TYPE zlest0061-hr_chegada.

  DATA: vl_aut TYPE sy-subrc.

  "Check de permissão de edição
  CLEAR: vl_aut.
  PERFORM check_autorizacao CHANGING vl_aut.

  CLEAR: gt_saida_nfe[].

  IF var_tp_alv = '1'.
    REFRESH: lt_zlest0063[], lt_zlest0060[], lt_makt[], gt_zlest0060[].
    REFRESH: gt_saida_viagem[].
    CLEAR: ls_zlest0063, ls_zlest0060, ls_makt.
    CLEAR: var_total.

    SELECT * FROM zlest0063
      INTO TABLE lt_zlest0063
    WHERE bukrs      EQ wp_tela_0100-wp_bukrs
      AND werks      EQ wp_tela_0100-wp_werks
      AND nr_viagem  EQ wp_tela_0100-wp_viagem
      AND ano_viagem EQ wp_tela_0100-wp_ano
      AND embarcacao EQ 'B'.

    CHECK NOT lt_zlest0063[] IS INITIAL.

    SELECT * FROM zlest0060
      INTO TABLE lt_zlest0060
      FOR ALL ENTRIES IN lt_zlest0063
    WHERE bukrs      EQ lt_zlest0063-bukrs
      AND werks      EQ lt_zlest0063-werks
      AND nr_viagem  EQ lt_zlest0063-nr_viagem
      AND ano_viagem EQ lt_zlest0063-ano_viagem.

    CHECK NOT lt_zlest0060[] IS INITIAL.

    SELECT * FROM zlest0061
      INTO TABLE lt_zlest0061
      FOR ALL ENTRIES IN lt_zlest0063
    WHERE bukrs      EQ lt_zlest0063-bukrs
      AND werks      EQ lt_zlest0063-werks
      AND nr_viagem  EQ lt_zlest0063-nr_viagem
      AND ano_viagem EQ lt_zlest0063-ano_viagem
      AND nome_emb   EQ lt_zlest0063-nome_emb.


    SELECT * FROM j_1bnfdoc
      INTO TABLE @DATA(lt_j_1bnfdoc)
      FOR ALL ENTRIES IN @lt_zlest0061
    WHERE docnum EQ @lt_zlest0061-docnum
      AND nfe    EQ 'X'
      AND cancel NE 'X'.


    LOOP AT lt_zlest0063 INTO ls_zlest0063.

      CLEAR: var_total, var_total_rat.
      CLEAR: var_data, var_hora.

      LOOP AT lt_zlest0061 INTO ls_zlest0061 WHERE bukrs      EQ ls_zlest0063-bukrs
                                               AND werks      EQ ls_zlest0063-werks
                                               AND nr_viagem  EQ ls_zlest0063-nr_viagem
                                               AND ano_viagem EQ ls_zlest0063-ano_viagem
                                               AND nome_emb   EQ ls_zlest0063-nome_emb.

        READ TABLE lt_j_1bnfdoc INTO DATA(ls_j_1bnfdoc) WITH KEY docnum = ls_zlest0061-docnum.
        IF sy-subrc = 0.

          gw_zlest0060-ck_anulado = ls_zlest0061-ck_anulado.
          var_total      = var_total     + ls_zlest0061-peso_vinculado.
          var_total_rat  = var_total_rat + ls_zlest0061-peso_chegada.

          IF var_data IS INITIAL.
            var_data       = ls_zlest0061-dt_chegada.
          ENDIF.

          IF var_hora IS INITIAL.
            var_hora       = ls_zlest0061-hr_chegada.
          ENDIF.

          IF gw_zlest0060-dt_descarga_ini IS INITIAL.
            gw_zlest0060-dt_descarga_ini = ls_zlest0061-dt_descarga_ini.
          ENDIF.

          IF gw_zlest0060-hr_descarga_ini IS INITIAL.
            gw_zlest0060-hr_descarga_ini = ls_zlest0061-hr_descarga_ini.
          ENDIF.

          IF gw_zlest0060-dt_descarga_fim IS INITIAL.
            gw_zlest0060-dt_descarga_fim = ls_zlest0061-dt_descarga_fim.
          ENDIF.

          IF gw_zlest0060-hr_descarga_fim IS INITIAL.
            gw_zlest0060-hr_descarga_fim = ls_zlest0061-hr_descarga_fim.
          ENDIF.

        ENDIF.
        CLEAR: ls_zlest0061.

      ENDLOOP.

      gw_zlest0060-peso_fiscal  = var_total.
      gw_zlest0060-peso_chegada = var_total_rat.
      gw_zlest0060-data_chegada = var_data.
      gw_zlest0060-hr_prevista  = var_hora.

      gw_zlest0060-bukrs        = ls_zlest0063-bukrs.
      gw_zlest0060-werks        = ls_zlest0063-werks.
      gw_zlest0060-nr_viagem    = ls_zlest0063-nr_viagem.
      gw_zlest0060-ano_viagem   = ls_zlest0063-ano_viagem.
      gw_zlest0060-nome_emb     = ls_zlest0063-nome_emb.

      APPEND gw_zlest0060 TO gt_zlest0060.

      CLEAR: gw_zlest0060, ls_zlest0063, var_total.

    ENDLOOP.

    SELECT * FROM makt
      INTO TABLE lt_makt
       FOR ALL ENTRIES IN lt_zlest0063
     WHERE matnr EQ lt_zlest0063-cod_material
       AND spras EQ 'PT'.

    LOOP AT lt_zlest0063 INTO ls_zlest0063.

      CLEAR: gw_saida_viagem, gt_estilo[].

      gw_saida_viagem-nome_emb        = ls_zlest0063-nome_emb.
      gw_saida_viagem-ano_viagem      = ls_zlest0063-ano_viagem.
      gw_saida_viagem-bukrs           = ls_zlest0063-bukrs.
      gw_saida_viagem-werks           = ls_zlest0063-werks.
      gw_saida_viagem-nr_viagem       = ls_zlest0063-nr_viagem.

      READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_zlest0063-cod_material.
      gw_saida_viagem-cod_material    = ls_makt-matnr.
      gw_saida_viagem-maktx           = ls_makt-maktx.


      CASE ls_zlest0063-tp_class.
        WHEN: 'CO'.
          gw_saida_viagem-tp_class = 'Convencional'.
        WHEN: 'R1'.
          gw_saida_viagem-tp_class = 'R1-RR'.
        WHEN: 'R2'.
          gw_saida_viagem-tp_class = 'R2-RR2'.
      ENDCASE.

*"// WBARBOSA 16102024 US-153329
      IF ls_zlest0063-eudr EQ gc_atende_eudr.
        gw_saida_viagem-ico_eudr = icon_checked.
      ELSE.
        CLEAR gw_saida_viagem-ico_eudr.
      ENDIF.
*"// WBARBOSA 16102024 US-153329

      READ TABLE gt_zlest0060 INTO gw_zlest0060 WITH KEY bukrs      = ls_zlest0063-bukrs
                                                         werks      = ls_zlest0063-werks
                                                         nr_viagem  = ls_zlest0063-nr_viagem
                                                         ano_viagem = ls_zlest0063-ano_viagem
                                                         nome_emb   = ls_zlest0063-nome_emb.

      gw_saida_viagem-peso_faturado   = gw_zlest0060-peso_fiscal.
      gw_saida_viagem-peso_descarga   = gw_zlest0060-peso_chegada.
      gw_saida_viagem-data_chegada    = gw_zlest0060-data_chegada.
      gw_saida_viagem-ck_anulado      = gw_zlest0060-ck_anulado.
      gw_saida_viagem-hr_prevista     = gw_zlest0060-hr_prevista.

      gw_saida_viagem-dt_descarga_ini = gw_zlest0060-dt_descarga_ini.
      gw_saida_viagem-hr_descarga_ini = gw_zlest0060-hr_descarga_ini.
      gw_saida_viagem-dt_descarga_fim = gw_zlest0060-dt_descarga_fim.
      gw_saida_viagem-hr_descarga_fim = gw_zlest0060-hr_descarga_fim.


      IF NOT ( gw_saida_viagem-data_chegada IS INITIAL ) AND NOT ( gw_saida_viagem-peso_descarga IS INITIAL ) OR
         vl_aut IS NOT INITIAL.

        gw_estilo-fieldname = 'CK_TERMINAL'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'DATA_CHEGADA'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'DT_DESCARGA_FIM'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'DT_DESCARGA_INI'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'HR_DESCARGA_FIM'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'HR_DESCARGA_INI'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'HR_PREVISTA'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gw_estilo TO gt_estilo.

        gw_estilo-fieldname = 'PESO_DESCARGA'.
        gw_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gw_estilo TO gt_estilo.


        INSERT LINES OF gt_estilo INTO TABLE gw_saida_viagem-estilo.

      ENDIF.

      READ TABLE lt_zlest0061 INTO DATA(wl_zlest0061) WITH KEY  bukrs      = ls_zlest0063-bukrs
                                                                werks      = ls_zlest0063-werks
                                                                nr_viagem  = ls_zlest0063-nr_viagem
                                                                ano_viagem = ls_zlest0063-ano_viagem
                                                                nome_emb   = ls_zlest0063-nome_emb.


      IF ( wl_zlest0061-dt_chegada_terminal GE '19400501' ). "( WL_ZLEST0061-DT_CHEGADA_TERMINAL IS NOT INITIAL ).
        gw_saida_viagem-ck_terminal = 'X'.
      ENDIF.

      gw_saida_viagem-visu            = icon_idoc.
      gw_saida_viagem-edit            = icon_change.


      APPEND gw_saida_viagem TO gt_saida_viagem.

      CLEAR: ls_zlest0063, ls_zlest0061, gw_zlest0060, ls_makt,  gw_saida_viagem, wl_zlest0061.
      REFRESH: gt_estilo[].

    ENDLOOP.

    CHECK NOT gt_saida_viagem[] IS INITIAL.
  ELSEIF var_tp_alv = '3'.
    CLEAR: gw_saida_viagem.
    PERFORM: selecionar_nfe USING gw_saida_viagem.
  ELSE.
    CLEAR: gw_saida_viagem.
    PERFORM: selecionar_cte USING gw_saida_viagem.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS_CHEGADA

*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_CHEGADA
*&---------------------------------------------------------------------*
FORM criar_alv_chegada.

  DATA: lt_toolbar_excluding TYPE ui_functions,
        ls_toolbar_excluding TYPE ui_func.

  DATA: ls_layout TYPE lvc_s_layo.

  PERFORM: criar_catalogo_viagem.

  IF NOT obj_custom_viagem IS INITIAL.
    CALL METHOD obj_custom_viagem->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
  ENDIF.

  IF NOT obj_grid_viagem IS INITIAL.
    CALL METHOD obj_grid_viagem->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
  ENDIF.

  CLEAR: obj_custom_viagem, obj_grid_viagem.

  CREATE OBJECT obj_custom_viagem
    EXPORTING
      container_name              = 'CONTAINER_CHEGADA'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CREATE OBJECT obj_grid_viagem
    EXPORTING
      i_parent          = obj_custom_viagem
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ls_toolbar_excluding = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_toolbar_excluding TO lt_toolbar_excluding.

  ls_toolbar_excluding = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_toolbar_excluding TO lt_toolbar_excluding.

  ls_toolbar_excluding = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_toolbar_excluding TO lt_toolbar_excluding.

  ls_toolbar_excluding = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_toolbar_excluding TO lt_toolbar_excluding.

  ls_toolbar_excluding = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_toolbar_excluding TO lt_toolbar_excluding.

  ls_toolbar_excluding = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_toolbar_excluding TO lt_toolbar_excluding.


  ls_layout-stylefname = 'ESTILO'.

  IF var_tp_alv = '1'.
*    IF ( OBJ_CUSTOM_VIAGEM IS INITIAL ).
    CALL METHOD obj_grid_viagem->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
*       I_SAVE                        = 'A'
        it_toolbar_excluding          = lt_toolbar_excluding
      CHANGING
        it_outtab                     = gt_saida_viagem[]
        it_fieldcatalog               = gt_fcat_viagem[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    obj_grid_viagem->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified  ).
    obj_grid_viagem->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter  ).

    SET HANDLER: lcl_event=>on_data_changed FOR obj_grid_viagem,
                 lcl_event=>on_data_changed_finish FOR obj_grid_viagem,
                 lcl_event=>hotspot_click   FOR obj_grid_viagem.
*    ELSE.
*
*      CALL METHOD OBJ_GRID_VIAGEM->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = WA_STABLE.
*
*    ENDIF.
  ELSEIF var_tp_alv = '2'.
*    IF ( OBJ_CUSTOM_VIAGEM IS INITIAL ).
    CALL METHOD obj_grid_viagem->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
*       I_SAVE                        = 'A'
        it_toolbar_excluding          = lt_toolbar_excluding
      CHANGING
        it_outtab                     = gt_saida_cte[]
        it_fieldcatalog               = gt_fcat_viagem[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

*    ELSE.
*
*      CALL METHOD OBJ_GRID_VIAGEM->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = WA_STABLE.

*    ENDIF.
  ELSEIF var_tp_alv = '3'.

    "CLEAR: LS_LAYOUT.
    ls_layout-sel_mode = 'A'.

    CREATE OBJECT obj_toolbar
      EXPORTING
        io_alv_grid = obj_grid_viagem.

    SET HANDLER: obj_toolbar->on_toolbar          FOR obj_grid_viagem,
                 obj_toolbar->handle_user_command FOR obj_grid_viagem,
                 lcl_event=>hotspot_click         FOR obj_grid_viagem.

    CALL METHOD obj_grid_viagem->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
        "I_SAVE                        = 'A'
        it_toolbar_excluding          = lt_toolbar_excluding
      CHANGING
        it_outtab                     = gt_saida_nfe[]
        it_fieldcatalog               = gt_fcat_viagem[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.



  ENDIF.

ENDFORM.                    " CRIAR_ALV_CHEGADA
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOGO_VIAGEM
*&---------------------------------------------------------------------*
FORM criar_catalogo_viagem .

  DATA: vl_aut TYPE sy-subrc.

  REFRESH gt_fcat_viagem.
  IF var_tp_alv = '1'.
    PERFORM montar_catalog_viagem USING:
          'VISU'          'Notas'            '10'   ''  'X' '' 'C' ''  ''  ''          ''            ''                '',
          'EDIT'          'Editar'           '10'   ''  'X' '' 'C' ''  ''  ''          ''            ''                '',
          'NOME_EMB'      'Barcaça'          '12'   ''  ''  '' ''  ''  ''  ''          ''            ''                '',
          'COD_MATERIAL'  'Cod.Produto'      '8'    'X' ''  '' ''  ''  ''  ''          ''            ''                '',
          'MAKTX'         'Desc.Produto'     '25'   ''  ''  '' ''  ''  ''  ''          ''            ''                '',
          'TP_CLASS'      'Class. Produto'   '13'   ''  ''  '' ''  ''  ''  ''          ''            ''                '',
*"// wbarbosa 16102024 US-153329
          'ICO_EUDR'      'Atende EUDR'      '12'   ''  ''  '' 'C'  ''  ''  ''          ''            ''                '',
*"// wbarbosa 16102024 US-153329
          'PESO_FATURADO' 'Peso Faturado'    '10'   ''  ''  '' ''  'X' ''  ''          ''            ''                '',
          'CK_TERMINAL'   'Chegada Terminal' '17'   ''  ''  '' ''  ''  'X' ''          ''            ''                'X',
          'CK_ANULADO'    'CT-e Anulado'     '10'   ''  ''  '' ''  ''  ''  ''          ''            ''                '',
          'DATA_CHEGADA'  'Data Chegada'     '13'   ''  ''  '' ''  ''  'X' 'ZLEST0061' 'DT_CHEGADA'  'GT_SAIDA_VIAGEM' '',
          'HR_PREVISTA'   'Hora Chegada'     '13'   ''  ''  '' ''  ''  'X' 'ZLEST0056' 'HR_PREVISTA' 'GT_SAIDA_VIAGEM' '',

          'DT_DESCARGA_INI'  'Dt.Descarga Ini.'  '16'   ''  ''  '' ''  ''  'X' 'ZLEST0061' 'DT_DESCARGA_INI'   'GT_SAIDA_VIAGEM' '',
          'HR_DESCARGA_INI'  'Hr.Descarga Ini.'  '16'   ''  ''  '' ''  ''  'X' 'ZLEST0061' 'HR_DESCARGA_INI'   'GT_SAIDA_VIAGEM' '',
          'DT_DESCARGA_FIM'  'Dt.Descarga Fim'   '15'   ''  ''  '' ''  ''  'X' 'ZLEST0061' 'DT_DESCARGA_FIM'   'GT_SAIDA_VIAGEM' '',
          'HR_DESCARGA_FIM'  'Hr.Descarga Fim'   '15'   ''  ''  '' ''  ''  'X' 'ZLEST0061' 'HR_DESCARGA_FIM'   'GT_SAIDA_VIAGEM' '',
          'PESO_DESCARGA'    'Peso Chegada'      '13'   ''  ''  '' ''  'X' 'X' '' ''         ''                ''.

  ELSEIF var_tp_alv = '3'.
    PERFORM montar_catalog_viagem USING:
      'NFNUM'         'Número'         '10'   ''  ''  '' ''   ''   ''  '' '' '' '',
      'DOCDAT'        'Dt.Emissão'     '10'   ''  ''  '' ''   ''   ''  '' '' '' '',
      'CHAVE_NFE'     'Chave'          '45'   ''  ''  '' ''   ''   ''  '' '' '' '',
      'RM_CODIGO'     'Cd.Remetente'   '10'   ''  ''  '' ''   ''   ''  'LFA1' 'LIFNR' '' '',
      'NAME1'         'Ds.Remetente'   '30'   ''  ''  '' ''   ''   ''  '' '' '' '',
      'MATNR'         'Cd.Material'    '10'   ''  ''  '' ''   ''   ''  'MARA' 'MATNR' '' '',
      'MAKTX'         'Ds.Material'    '30'   ''  ''  '' ''   ''   ''  '' '' '' '',
      'DT_CHEGADA'    'Dt.Chegada'     '10'   ''  ''  '' ''   ''   ''  '' '' '' '',
      'HR_CHEGADA'    'Hr.Chegada'     '10'   ''  ''  '' ''   ''   ''  '' '' '' '',
      'PESO_CHEGADA'  'Peso Chegada'   '13'   ''  ''  '' ''   'X'  ''  '' '' '' '',
      'PESO_FISCAL'   'Peso Fiscal'    '13'   ''  ''  '' ''   'X'  ''  '' '' '' '',
      'TP_TRANSGENIA' 'Class. Produto' '13'   ''  ''  '' ''   ' '  ''  '' '' '' '',
*"// wbarbosa 16102024 US-153329
      'ICO_EUDR'      'Atende EUDR'    '12'   ''  ''  '' 'C'  ''   ''  '' '' '' '',
*"// wbarbosa 16102024 US-153329
      'NOME_EMB'      'Barcaça'        '12'   ''  ''  '' ''   ''   ''  '' '' '' '',
      'NUM_CTE'       'Ct-e'           '09'    ''  ''  '' ''   ''   ''  '' '' '' '',
      'VINC_TOT'      'Vinc.Tot.'      '10'   ''  ''  '' 'C'  ''   ''  '' '' '' '',
      'DISP_CCT'      'Disp.CCT'       '10'   ''  ''  '' 'C'  ''   ''  '' '' '' '',
*** Inicio - Rubenilson Pereira - 16.06.2022 - 54384
      'RECEP_CCT'     'Recep.CCT'      '10'   ''  ''  '' 'C'  ''   ''  '' '' '' '',
*** Fim - Rubenilson Pereira - 16.06.2022 - 54384
      'SEQ_LCTO_IND'  'Lcto.ZNFW Ind.' '15'   ''  'X'  '' 'C'  ''   ''  '' '' '' '',
      'NFENUM_IND'    'NF-e Ind.'      '10'   ''  'X'  '' 'C'  ''   ''  '' '' '' '',
      'DOCNUM_IND'    'Docnum Ind.'    '11'   ''  'X'  '' 'C'  ''   ''  '' '' '' '',
      'BELNR_IND'     'Doc.Ctb.Ind.'   '12'   ''  ' '  '' 'C'  ''   ''  '' '' '' ''.


  ELSE.
    PERFORM montar_catalog_viagem USING:
          'NFENUM'        'Ct-e'            '8'    ''  ''  '' ''  ''  ''  '' '' '' '',
          'CL_CODIGO'     'Cód. Cliente'    '10'   ''  ''  '' ''  ''  ''  '' '' '' '',
*          'VISU'          'Notas'           '10'   ''  'X' '' 'C' ''  ''  ''          '' '' '',
*          'EDIT'          'Editar'          '10'   ''  'X' '' 'C' ''  ''  ''          '' '' '',
          'NOME_EMB'      'Barcaça'         '12'   ''  ''  '' ''  ''  ''  '' '' '' '',
          'COD_MATERIAL'  'Cod.Produto'     '8'    'X' ''  '' ''  ''  ''  '' '' '' '',
          'MAKTX'         'Desc.Produto'    '25'   ''  ''  '' ''  ''  ''  '' '' '' '',
          'TP_CLASS'      'Class. Produto'  '13'   ''  ''  '' ''  ''  ''  '' '' '' '',
*"// wbarbosa 16102024 US-153329
          'ICO_EUDR'      'Atende EUDR'     '12'   ''  ''  '' 'C' ''  ''  '' '' '' '',
*"// wbarbosa 16102024 US-153329
          'PESO_FATURADO' 'Peso Faturado'   '10'   ''  ''  '' ''  'X' ''  '' '' '' '',
          'DATA_FATURA'   'Data Fatura'     '13'   ''  ''  '' ''  ''  ''  '' '' '' '',
          'CK_ANULADO'    'CT-e Anulado'    '10'   ''  ''  '' ''  ''  ''  '' '' '' '',
          'DATA_CHEGADA'  'Data Chegada'    '13'   ''  ''  '' ''  ''  ''  '' '' '' '',
          'HR_PREVISTA'   'Hora Chegada'    '13'   ''  ''  '' ''  ''  ''  '' '' '' '',
          'PESO_DESCARGA' 'Peso Chegada'    '13'   ''  ''  '' ''  'X' ''  '' '' '' ''.
  ENDIF.

***/// CS2021000269 Travas no lançamento de Barcaças ZLES0088 - Set/2021 - Inicio  - RMB
  "Check de permissão de edição
*  CLEAR: vl_aut.
*  PERFORM check_autorizacao CHANGING vl_aut.
*
*  IF vl_aut <> 0 AND var_tp_alv = '1'.
*    DELETE gt_fcat_viagem WHERE fieldname = 'EDIT' .
*    "IF sy-subrc = 0.
*    "  <fs_fcat>-no_out = 'X'.
*    "ENDIF.
*  ENDIF.
***/// CS2021000269 Travas no lançamento de Barcaças ZLES0088 - Set/2021 - Fim - RMB
ENDFORM.                    " CRIAR_CATALOGO_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_VIAGEM
*&---------------------------------------------------------------------*
FORM montar_catalog_viagem USING     VALUE(p_fieldname)
                                     VALUE(p_desc)
                                     VALUE(p_tam)
                                     VALUE(p_no_zero)
                                     VALUE(p_hotspot)
                                     VALUE(p_cor)
                                     VALUE(p_just)
                                     VALUE(p_sum)
                                     VALUE(p_edit)
                                     VALUE(p_ref_tabname)   LIKE dd02d-tabname
                                     VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).

  CLEAR: gw_fcat_viagem.

  gw_fcat_viagem-fieldname = p_fieldname.
  gw_fcat_viagem-ref_table = p_ref_tabname..
  gw_fcat_viagem-ref_field = p_ref_fieldname.
  gw_fcat_viagem-tabname   = p_tabname.
  gw_fcat_viagem-scrtext_l = p_desc.
  gw_fcat_viagem-scrtext_m = p_desc.
  gw_fcat_viagem-scrtext_s = p_desc.
  gw_fcat_viagem-outputlen = p_tam.
  gw_fcat_viagem-no_zero   = p_no_zero.
  gw_fcat_viagem-hotspot   = p_hotspot.
  gw_fcat_viagem-emphasize = p_cor.
  gw_fcat_viagem-just      = p_just.
  gw_fcat_viagem-do_sum    = p_sum.
  gw_fcat_viagem-edit      = p_edit.
  gw_fcat_viagem-checkbox  = p_check.

*  CASE P_TABNAME.
*    WHEN 'GT_SAIDA_VIAGEM'.
*      IF P_FIELDNAME EQ 'PESO_DESCARGA'.
*        GW_FCAT_VIAGEM-DECIMALS_O = '0'.
*      ENDIF.
*  ENDCASE.

  APPEND gw_fcat_viagem TO gt_fcat_viagem.

ENDFORM.                    " MONTAR_CATALOG_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  ABRIR_CTE_NOTAS
*&---------------------------------------------------------------------*
FORM abrir_cte_notas  USING p_saida_viagem TYPE ty_saida_viagem.


  CONSTANTS: var_x TYPE c LENGTH 1 VALUE 'X'.

  DATA: lt_zlest0061 TYPE TABLE OF zlest0061,
        ls_zlest0061 TYPE zlest0061.

  DATA: lt_zlest0060 TYPE TABLE OF zlest0060,
        ls_zlest0060 TYPE zlest0060.

  DATA: lt_j_1bnfdoc TYPE TABLE OF j_1bnfdoc,
        ls_j_1bnfdoc TYPE j_1bnfdoc.


  DATA: lt_j_1bnfe_active TYPE TABLE OF j_1bnfe_active,
        ls_j_1bnfe_active TYPE j_1bnfe_active.

  DATA: var_tabix TYPE sy-tabix.

  REFRESH: lt_zlest0061[], lt_zlest0060[], lt_j_1bnfdoc[], lt_j_1bnfe_active[].
  REFRESH: gt_organiza_tree[], gt_saida_tree[].


  SELECT * FROM zlest0061
    INTO TABLE lt_zlest0061
  WHERE bukrs      EQ p_saida_viagem-bukrs
    AND werks      EQ p_saida_viagem-werks
    AND ano_viagem EQ p_saida_viagem-ano_viagem
    AND nr_viagem  EQ p_saida_viagem-nr_viagem
    AND nome_emb   EQ p_saida_viagem-nome_emb.

  CHECK NOT lt_zlest0061[] IS INITIAL.

  SELECT * FROM j_1bnfdoc
    INTO TABLE lt_j_1bnfdoc
    FOR ALL ENTRIES IN lt_zlest0061
  WHERE docnum EQ lt_zlest0061-docnum
    AND nfe    EQ var_x.

  CHECK NOT lt_j_1bnfdoc[] IS INITIAL.

  SELECT * FROM j_1bnfe_active
    INTO TABLE lt_j_1bnfe_active
    FOR ALL ENTRIES IN lt_j_1bnfdoc
  WHERE docnum EQ lt_j_1bnfdoc-docnum
    AND cancel NE var_x.

  CHECK NOT lt_j_1bnfe_active[] IS INITIAL.

  LOOP AT lt_zlest0061 INTO ls_zlest0061.

    CLEAR: var_tabix.
    var_tabix = sy-tabix.

    READ TABLE lt_j_1bnfdoc INTO ls_j_1bnfdoc WITH KEY docnum = ls_zlest0061-docnum.
    IF ( sy-subrc EQ 0 ) AND NOT ( ls_j_1bnfdoc-nfenum IS INITIAL ).
      READ TABLE lt_j_1bnfe_active INTO ls_j_1bnfe_active WITH KEY docnum = ls_j_1bnfdoc-docnum.
      IF ( sy-subrc EQ 0 ).

        gw_organiza_tree-docnum    = ls_j_1bnfe_active-docnum.
        gw_organiza_tree-nfe_cte   = ls_j_1bnfdoc-nfenum.
        gw_organiza_tree-cl_codigo = ls_zlest0061-cl_codigo.

        APPEND gw_organiza_tree TO gt_organiza_tree.

      ELSE.
        DELETE lt_zlest0061 INDEX var_tabix.
        CLEAR: ls_zlest0061, ls_j_1bnfdoc, ls_j_1bnfe_active, gw_organiza_tree.
        CONTINUE.
      ENDIF.
    ELSE.
      DELETE lt_zlest0061 INDEX var_tabix.
      CLEAR: ls_zlest0061, ls_j_1bnfdoc, ls_j_1bnfe_active, gw_organiza_tree.
      CONTINUE.
    ENDIF.

    CLEAR: ls_zlest0061, ls_j_1bnfdoc, ls_j_1bnfe_active, gw_organiza_tree.
  ENDLOOP.

  CHECK NOT gt_organiza_tree[] IS INITIAL.

  "Coloca as Outras Linhas.
  SELECT * FROM zlest0060
    INTO TABLE lt_zlest0060
    FOR ALL ENTRIES IN lt_zlest0061
  WHERE bukrs         EQ lt_zlest0061-bukrs
    AND werks         EQ lt_zlest0061-werks
    AND ano_viagem    EQ lt_zlest0061-ano_viagem
    AND embarcacao    EQ lt_zlest0061-embarcacao
    AND nome_emb      EQ lt_zlest0061-nome_emb
    AND safra         EQ lt_zlest0061-safra
    AND cl_codigo     EQ lt_zlest0061-cl_codigo
    AND id_frete_aqua EQ lt_zlest0061-id_frete_aqua.

  IF ( sy-subrc EQ 0 ).

    LOOP AT lt_zlest0060 INTO ls_zlest0060.

      gw_organiza_tree-cl_codigo       = ls_zlest0060-cl_codigo.
      gw_organiza_tree-nfnum           = ls_zlest0060-nfnum.
      gw_organiza_tree-series          = ls_zlest0060-series.
      gw_organiza_tree-peso_fiscal     = ls_zlest0060-peso_fiscal.
      gw_organiza_tree-netwr           = ls_zlest0060-netwr.
      gw_organiza_tree-nr_romaneio     = ls_zlest0060-nr_romaneio.
      gw_organiza_tree-safra           = ls_zlest0060-safra.
      gw_organiza_tree-nr_dco          = ls_zlest0060-nr_dco.
      gw_organiza_tree-docnum          = ls_zlest0060-docnum.

      APPEND gw_organiza_tree TO gt_organiza_tree.

    ENDLOOP.

  ENDIF.


ENDFORM.                    " ABRIR_CTE_NOTAS
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.

  SET PF-STATUS 'PF0200'. "Status da Tela 0200.
  SET TITLEBAR  'TB0200' WITH var_title_popup. "Title da Tela 0200

  PERFORM: criar_alv_tree.

ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0200 INPUT.

  CASE sy-ucomm.
    WHEN: 'FECHAR'.

      IF NOT ( obj_grid_tree IS INITIAL ).

        CALL METHOD obj_grid_tree->delete_all_nodes.
        CALL METHOD obj_grid_tree->frontend_update.

        CLEAR: obj_grid_tree.
        CLEAR: obj_custom_tree.
        REFRESH: gt_catalog_tree.

      ENDIF.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_TREE
*&---------------------------------------------------------------------*
FORM criar_alv_tree .

  DATA: ls_variant TYPE disvariant.

  CLEAR: ls_variant.
  REFRESH: gt_catalog_tree[].
  CLEAR: obj_custom_tree, obj_grid_tree.

  PERFORM: catalog_tree.

  CREATE OBJECT obj_custom_tree
    EXPORTING
      container_name              = 'CONTAINER_TREE'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.
  ENDIF.

  CREATE OBJECT obj_grid_tree
    EXPORTING
      parent                      = obj_custom_tree
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
      item_selection              = 'X'
      no_html_header              = 'X'
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.
  ENDIF.

  PERFORM: criar_header CHANGING gt_header.

  ls_variant-report = sy-repid.

  CALL METHOD obj_grid_tree->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = gt_header
      i_save              = 'A'
      is_variant          = ls_variant
    CHANGING
      it_outtab           = gt_saida_tree
      it_fieldcatalog     = gt_catalog_tree.

  PERFORM: criar_hierarquia.


ENDFORM.                    " CRIAR_ALV_TREE
*&---------------------------------------------------------------------*
*&      Form  CATALOG_TREE
*&---------------------------------------------------------------------*
FORM catalog_tree .

  DATA: ls_catalog_tree TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZTREE_AQUA'
    CHANGING
      ct_fieldcat            = gt_catalog_tree
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SORT: gt_catalog_tree   BY scrtext_l.

  LOOP AT gt_catalog_tree INTO ls_catalog_tree.

    CASE ls_catalog_tree-fieldname.
      WHEN: 'NFE_CTE'.
        ls_catalog_tree-scrtext_l = 'Ct-e'.
        ls_catalog_tree-scrtext_m = 'Ct-e'.
        ls_catalog_tree-scrtext_s = 'Ct-e'.
        ls_catalog_tree-no_zero   = 'X'.
      WHEN: 'CL_CODIGO'.
        ls_catalog_tree-outputlen = '10'.
        ls_catalog_tree-scrtext_l = 'Cliente'.
        ls_catalog_tree-scrtext_m = 'Cliente'.
        ls_catalog_tree-scrtext_s = 'Cliente'.
      WHEN: 'PESO_FATURADO'.
        ls_catalog_tree-scrtext_l = 'Peso Faturado'.
        ls_catalog_tree-scrtext_m = 'Peso Faturado'.
        ls_catalog_tree-scrtext_s = 'Peso Faturado'.
      WHEN: 'PESO_CHEGADA'.
        ls_catalog_tree-scrtext_l = 'Peso Chegada'.
        ls_catalog_tree-scrtext_m = 'Peso Chegada'.
        ls_catalog_tree-scrtext_s = 'Peso Chegada'.
      WHEN: 'NFNUM'.
        ls_catalog_tree-outputlen = '10'.
        ls_catalog_tree-scrtext_l = 'Nf-e'.
        ls_catalog_tree-scrtext_m = 'Nf-e'.
        ls_catalog_tree-scrtext_s = 'Nf-e'.
        ls_catalog_tree-no_zero   = 'X'.
      WHEN: 'SERIES'.
        ls_catalog_tree-outputlen = '10'.
        ls_catalog_tree-scrtext_l = 'Serie'.
        ls_catalog_tree-scrtext_m = 'Serie'.
        ls_catalog_tree-scrtext_s = 'Serie'.
      WHEN: 'PESO_FISCAL'.
        ls_catalog_tree-scrtext_l = 'Peso'.
        ls_catalog_tree-scrtext_m = 'Peso'.
        ls_catalog_tree-scrtext_s = 'Peso'.
        ls_catalog_tree-do_sum    = 'X'.
      WHEN: 'NETWR'.
        ls_catalog_tree-scrtext_l = 'Valor'.
        ls_catalog_tree-scrtext_m = 'Valor'.
        ls_catalog_tree-scrtext_s = 'Valor'.
      WHEN: 'NR_ROMANEIO'.
        ls_catalog_tree-outputlen = '10'.
        ls_catalog_tree-scrtext_l = 'Romaneio'.
        ls_catalog_tree-scrtext_m = 'Romaneio'.
        ls_catalog_tree-scrtext_s = 'Romaneio'.
      WHEN: 'SAFRA'.
        ls_catalog_tree-outputlen = '10'.
        ls_catalog_tree-scrtext_l = 'Safra'.
        ls_catalog_tree-scrtext_m = 'Safra'.
        ls_catalog_tree-scrtext_s = 'Safra'.
      WHEN: 'NR_DCO'.
        ls_catalog_tree-outputlen = '10'.
        ls_catalog_tree-scrtext_l = 'DCO'.
        ls_catalog_tree-scrtext_m = 'DCO'.
        ls_catalog_tree-scrtext_s = 'DCO'.
      WHEN: 'DOCNUM'.
        ls_catalog_tree-outputlen = '10'.
        ls_catalog_tree-scrtext_l = 'Docnum'.
        ls_catalog_tree-scrtext_m = 'Docnum'.
        ls_catalog_tree-scrtext_s = 'Docnum'.
    ENDCASE.


    MODIFY gt_catalog_tree FROM ls_catalog_tree.
  ENDLOOP.

ENDFORM.                    " CATALOG_TREE
*&---------------------------------------------------------------------*
*&      Form  CRIAR_HEADER
*&---------------------------------------------------------------------*
FORM criar_header  CHANGING p_header TYPE treev_hhdr.

  p_header-heading = 'Ct-e'.
  p_header-tooltip = 'Centro de Custo'.
  p_header-width = 30.
  p_header-width_pix = ''.

ENDFORM.                    " CRIAR_HEADER
*&---------------------------------------------------------------------*
*&      Form  CRIAR_HIERARQUIA
*&---------------------------------------------------------------------*
FORM criar_hierarquia .


  DATA: cte_key   TYPE lvc_nkey,
        notas_key TYPE lvc_nkey,
        last_key  TYPE lvc_nkey.


  DATA: lt_organiza_tree TYPE TABLE OF ty_organiza_tree,
        ls_organiza_tree TYPE ty_organiza_tree.

  REFRESH: lt_organiza_tree[].
  CLEAR: ls_organiza_tree, cte_key, notas_key.

  lt_organiza_tree[] = gt_organiza_tree[].

  DELETE lt_organiza_tree WHERE nfe_cte NE space.

  LOOP AT gt_organiza_tree INTO gw_organiza_tree.

    IF ( gw_organiza_tree-nfe_cte IS INITIAL ).
      CONTINUE.
    ENDIF.


    ON CHANGE OF gw_organiza_tree-nfe_cte.
      PERFORM add_nfe_cte USING gw_organiza_tree
                                ''
                                CHANGING cte_key.
    ENDON.



    LOOP AT lt_organiza_tree INTO ls_organiza_tree WHERE docnum EQ gw_organiza_tree-docnum.



      gw_saida_tree-cl_codigo   = ls_organiza_tree-cl_codigo.
      gw_saida_tree-nfnum       = ls_organiza_tree-nfnum.
      gw_saida_tree-series      = ls_organiza_tree-series.
      gw_saida_tree-peso_fiscal = ls_organiza_tree-peso_fiscal.
      gw_saida_tree-netwr       = ls_organiza_tree-netwr.
      gw_saida_tree-nr_romaneio = ls_organiza_tree-nr_romaneio.
      gw_saida_tree-safra       = ls_organiza_tree-safra.
      gw_saida_tree-nr_dco      = ls_organiza_tree-nr_dco.
      gw_saida_tree-docnum      = ls_organiza_tree-docnum.


      IF NOT ( ls_organiza_tree-nfnum IS INITIAL ).

        PERFORM add_notas USING   gw_saida_tree
                                  cte_key
                          CHANGING notas_key.
      ENDIF.

    ENDLOOP.

    CLEAR: ls_organiza_tree, gw_saida_tree.

  ENDLOOP.

  CALL METHOD obj_grid_tree->update_calculations.
  CALL METHOD obj_grid_tree->frontend_update.

ENDFORM.                    " CRIAR_HIERARQUIA
*&---------------------------------------------------------------------*
*&      Form  ADD_NFE_CTE
*&---------------------------------------------------------------------*
FORM add_nfe_cte  USING    p_organiza_tree TYPE ty_organiza_tree
                           p_relat_key     TYPE lvc_nkey
                  CHANGING p_node_key      TYPE lvc_nkey.




  DATA: l_node_text    TYPE lvc_value,
        lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi,
        ls_node        TYPE lvc_s_layn.

  "LS_ITEM_LAYOUT-T_IMAGE   = '@3P@'.
*  LS_ITEM_LAYOUT-FIELDNAME = TREE->C_HIERARCHY_COLUMN_NAME.
*
*  APPEND LS_ITEM_LAYOUT TO LT_ITEM_LAYOUT.

  l_node_text       =  p_organiza_tree-nfe_cte.
  ls_node-n_image   = '@FN@'.
  ls_node-exp_image = space.

  CALL METHOD obj_grid_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_node_layout   = ls_node
      is_outtab_line   = gw_saida_tree
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " ADD_NFE_CTE
*&---------------------------------------------------------------------*
*&      Form  ADD_NOTAS
*&---------------------------------------------------------------------*
FORM add_notas  USING    p_saida_tree TYPE ty_saida_tree
                         p_cte_key    TYPE lvc_nkey
                CHANGING p_notas_key  TYPE lvc_nkey.


  DATA: l_node_text    TYPE lvc_value,
        lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  "LS_ITEM_LAYOUT-T_IMAGE   = '@3Y@'.
  "LS_ITEM_LAYOUT-STYLE     =    CL_GUI_COLUMN_TREE=>STYLE_INTENSIFIED.
  ls_item_layout-fieldname = obj_grid_tree->c_hierarchy_column_name.
  APPEND ls_item_layout TO lt_item_layout.

  l_node_text =  p_saida_tree-nfnum.

  CALL METHOD obj_grid_tree->add_node
    EXPORTING
      i_relat_node_key = p_cte_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = p_saida_tree
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_notas_key.



ENDFORM.                    " ADD_NOTAS
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM preencher_campos .

  DATA: wl_t001  TYPE t001,  " Work Area para Informações da Empresa
        wl_t001w TYPE t001w. " Work Area para Informações do Centro

  CLEAR: wl_t001, wl_t001w. "Limpar Work Area

  "Recuperar Descrição da Empresa
  SELECT SINGLE * FROM t001 INTO  wl_t001  WHERE bukrs EQ wp_tela_0100-wp_bukrs.
  wp_tela_0100-w_desc_bukrs = wl_t001-butxt. "Descrição da Empresa
  "Recuperar Descrição do Centro Emissor
  SELECT SINGLE * FROM t001w INTO wl_t001w WHERE werks EQ wp_tela_0100-wp_werks.
  wp_tela_0100-w_desc_werks = wl_t001w-name1. "Descrição do Centro

  CLEAR: wp_tela_0100-dt_sai_comboio,wp_tela_0100-hr_sai_comboio.

  SELECT SINGLE *
    FROM zlest0068 INTO @DATA(_wl_0068)
   WHERE bukrs       = @wp_tela_0100-wp_bukrs
     AND werks       = @wp_tela_0100-wp_werks
     AND ano_viagem  = @wp_tela_0100-wp_ano
     AND nr_viagem   = @wp_tela_0100-wp_viagem.

  IF sy-subrc EQ 0.
    wp_tela_0100-dt_sai_comboio  = _wl_0068-dt_saida_comboio.
    wp_tela_0100-hr_sai_comboio  = _wl_0068-hr_saida_comboio.
  ENDIF.


ENDFORM.                    " PREENCHER_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_NR_VIAGEM
*&---------------------------------------------------------------------*
FORM selecionar_nr_viagem.

  DATA: lt_zlest0068 TYPE TABLE OF zlest0068,
        ls_zlest0068 TYPE zlest0058.

  REFRESH: lt_zlest0068[],
           gt_saida_nr_viagem[].

  SELECT * FROM zlest0068
    INTO TABLE lt_zlest0068
        WHERE bukrs      EQ wp_tela_0100-wp_bukrs
          AND werks      EQ wp_tela_0100-wp_werks
          AND ano_viagem EQ wp_tela_0100-wp_ano.

  CHECK NOT lt_zlest0068[] IS INITIAL.

  LOOP AT lt_zlest0068 INTO ls_zlest0068.

    gw_saida_nr_viagem-bukrs         =  ls_zlest0068-bukrs.
    gw_saida_nr_viagem-werks         =  ls_zlest0068-werks.
    gw_saida_nr_viagem-ano_viagem    =  ls_zlest0068-ano_viagem.
    gw_saida_nr_viagem-nr_viagem     =  ls_zlest0068-nr_viagem.

    APPEND gw_saida_nr_viagem TO gt_saida_nr_viagem.

    CLEAR: gw_saida_nr_viagem.

  ENDLOOP.

  SORT: gt_saida_nr_viagem BY nr_viagem DESCENDING.

  CALL SCREEN 0300 STARTING AT 15 1 ENDING AT 95 10.

ENDFORM.                    " SELECIONAR_NR_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_NR_VIAGEM
*&---------------------------------------------------------------------*
FORM criar_alv_nr_viagem .
  DATA: wl_layout   TYPE lvc_s_layo.

  CLEAR: wl_layout.
  REFRESH: gt_fcat_nr_viagem.
  CLEAR: gw_fcat_nr_viagem.

  PERFORM montar_catalog_viagens USING:
       'BUKRS'      'Empresa'    '15'   '' '' '' '' '' '' '' '' '' ,
       'WERKS'      'Centro'     '15'   '' '' '' '' '' '' '' '' '' ,
       'ANO_VIAGEM' 'Ano Viagem' '15'   '' '' '' '' '' '' '' '' '' ,
       'NR_VIAGEM'  'Nr. Viagem' '15'   '' '' '' '' '' '' '' '' ''.

  IF ( obj_custom_nr_viagem IS INITIAL ).

    CREATE OBJECT obj_custom_nr_viagem
      EXPORTING
        container_name              = 'CONTAINER_NR_VIAGEM'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT obj_grid_nr_viagem
      EXPORTING
        i_parent          = obj_custom_nr_viagem
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    wl_layout-sel_mode   = 'A'.
    wl_layout-no_toolbar = 'X'.

    CALL METHOD obj_grid_nr_viagem->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        i_save                        = 'U'
      CHANGING
        it_outtab                     = gt_saida_nr_viagem[]
        it_fieldcatalog               = gt_fcat_nr_viagem[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD obj_grid_nr_viagem->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_NR_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_VIAGENS
*&---------------------------------------------------------------------*
FORM montar_catalog_viagens  USING  VALUE(p_fieldname)
                                   VALUE(p_desc)
                                   VALUE(p_tam)
                                   VALUE(p_no_zero)
                                   VALUE(p_hotspot)
                                   VALUE(p_cor)
                                   VALUE(p_just)
                                   VALUE(p_sum)
                                   VALUE(p_edit)
                                   VALUE(p_ref_tabname)   LIKE dd02d-tabname
                                   VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                                   VALUE(p_tabname)       LIKE dd02d-tabname.

  CLEAR: gw_fcat_nr_viagem.

  gw_fcat_nr_viagem-fieldname = p_fieldname.
  gw_fcat_nr_viagem-ref_table = p_ref_tabname..
  gw_fcat_nr_viagem-ref_field = p_ref_fieldname.
  gw_fcat_nr_viagem-tabname   = p_tabname.
  gw_fcat_nr_viagem-scrtext_l = p_desc.
  gw_fcat_nr_viagem-scrtext_m = p_desc.
  gw_fcat_nr_viagem-scrtext_s = p_desc.
  gw_fcat_nr_viagem-outputlen = p_tam.
  gw_fcat_nr_viagem-no_zero   = p_no_zero.
  gw_fcat_nr_viagem-hotspot   = p_hotspot.
  gw_fcat_nr_viagem-emphasize = p_cor.
  gw_fcat_nr_viagem-just      = p_just.
  gw_fcat_nr_viagem-do_sum    = p_sum.
  gw_fcat_nr_viagem-edit      = p_edit.

  APPEND gw_fcat_nr_viagem TO gt_fcat_nr_viagem.


ENDFORM.                    " MONTAR_CATALOG_VIAGENS
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR  'TB0300'.
  PERFORM: criar_alv_nr_viagem.
  PERFORM: preencher_campos.


ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
MODULE pai_0300 INPUT.


  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  CASE sy-ucomm.
    WHEN: 'OK_NR'.

      CALL METHOD obj_grid_nr_viagem->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      READ TABLE tl_rows INTO sl_rows INDEX 1.

      IF ( sy-subrc EQ 0 ).

        READ TABLE
          gt_saida_nr_viagem
          INTO gw_saida_nr_viagem
          INDEX sl_rows-index.

        wp_tela_0100-wp_viagem = gw_saida_nr_viagem-nr_viagem.

        PERFORM: preencher_campos.

        PERFORM: seleciona_dados_chegada.

      ENDIF.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.                 " PAI_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.
  SET TITLEBAR 'TB0400'.
  SET PF-STATUS 'PF0400'.
  PERFORM: criar_alv_cte.
ENDMODULE.                 " PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0400  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0400 INPUT.
  CASE sy-ucomm.
    WHEN: 'OK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " PAI_0400  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_CTE
*&---------------------------------------------------------------------*
FORM selecionar_cte USING p_saida_viagem TYPE ty_saida_viagem .

  DATA: lt_zlest0061 TYPE TABLE OF zlest0061,
        ls_zlest0061 TYPE zlest0061,

        lt_zlest0060 TYPE TABLE OF zlest0060,
        ls_zlest0060 TYPE zlest0060,

        lt_zlest0063 TYPE TABLE OF zlest0063,
        ls_zlest0063 TYPE zlest0063,

        lt_makt      TYPE TABLE OF makt,
        ls_makt      TYPE makt.

  DATA: lt_j_1bnfdoc TYPE TABLE OF j_1bnfdoc,
        ls_j_1bnfdoc TYPE j_1bnfdoc.

  DATA: lt_j_1bnfe_active TYPE TABLE OF j_1bnfe_active,
        ls_j_1bnfe_active TYPE j_1bnfe_active.

  REFRESH: gt_saida_cte[].
  CLEAR: gw_saida_cte.

  IF var_tp_alv = '1'.
    SELECT * FROM zlest0061
      INTO TABLE lt_zlest0061
      WHERE bukrs    EQ p_saida_viagem-bukrs
      AND werks      EQ p_saida_viagem-werks
      AND ano_viagem EQ p_saida_viagem-ano_viagem
      AND nr_viagem  EQ p_saida_viagem-nr_viagem
      AND nome_emb   EQ p_saida_viagem-nome_emb.
  ELSEIF var_tp_alv = '2'.
    SELECT * FROM zlest0061
      INTO TABLE lt_zlest0061
      WHERE bukrs    EQ wp_tela_0100-wp_bukrs
      AND werks      EQ wp_tela_0100-wp_werks
      AND ano_viagem EQ wp_tela_0100-wp_ano
      AND nr_viagem  EQ wp_tela_0100-wp_viagem.

    SELECT * FROM zlest0060
      INTO TABLE lt_zlest0060
       FOR ALL ENTRIES IN lt_zlest0061
     WHERE bukrs         EQ lt_zlest0061-bukrs
       AND werks         EQ lt_zlest0061-werks
       AND nr_viagem     EQ lt_zlest0061-nr_viagem
       AND ano_viagem    EQ lt_zlest0061-ano_viagem
       AND id_frete_aqua EQ lt_zlest0061-id_frete_aqua.

    SELECT * FROM zlest0063
      INTO TABLE lt_zlest0063
       FOR ALL ENTRIES IN lt_zlest0061
     WHERE bukrs      EQ lt_zlest0061-bukrs
       AND werks      EQ lt_zlest0061-werks
       AND nr_viagem  EQ lt_zlest0061-nr_viagem
       AND ano_viagem EQ lt_zlest0061-ano_viagem
       AND embarcacao EQ 'B'.

    SELECT * FROM makt
      INTO TABLE lt_makt
       FOR ALL ENTRIES IN lt_zlest0063
     WHERE matnr EQ lt_zlest0063-cod_material
       AND spras EQ 'PT'.
  ENDIF.

  CHECK NOT lt_zlest0061[] IS INITIAL.

  SELECT * FROM j_1bnfdoc
    INTO TABLE lt_j_1bnfdoc
    FOR ALL ENTRIES IN lt_zlest0061
  WHERE docnum EQ lt_zlest0061-docnum
    AND nfe    EQ 'X'
    AND cancel NE 'X'.

  CHECK NOT lt_j_1bnfdoc[] IS INITIAL.

  SELECT * FROM j_1bnfe_active
    INTO TABLE lt_j_1bnfe_active
    FOR ALL ENTRIES IN lt_j_1bnfdoc
  WHERE docnum EQ lt_j_1bnfdoc-docnum
    AND cancel NE 'X'.

  CHECK NOT lt_j_1bnfe_active[] IS INITIAL.

  IF var_tp_alv = '1'.
    LOOP AT lt_zlest0061 INTO ls_zlest0061.

      READ TABLE lt_j_1bnfdoc INTO ls_j_1bnfdoc WITH KEY docnum = ls_zlest0061-docnum.
      IF ( sy-subrc EQ 0 ) AND NOT ( ls_j_1bnfdoc-nfenum IS INITIAL ).
        READ TABLE lt_j_1bnfe_active INTO ls_j_1bnfe_active WITH KEY docnum = ls_j_1bnfdoc-docnum.
        IF ( sy-subrc EQ 0 ).

          gw_saida_cte-nfenum       = ls_j_1bnfdoc-nfenum.
          gw_saida_cte-cl_codigo    = ls_zlest0061-cl_codigo.
          gw_saida_cte-peso_fiscal  = ls_zlest0061-peso_vinculado.
          gw_saida_cte-peso_chegada = ls_zlest0061-peso_chegada.
          gw_saida_cte-ck_anulado   = ls_zlest0061-ck_anulado.

          APPEND gw_saida_cte TO gt_saida_cte.

          CLEAR: ls_zlest0061, ls_j_1bnfdoc, ls_j_1bnfe_active, gw_saida_cte.
        ENDIF.
      ENDIF.
      CLEAR: ls_zlest0061, ls_j_1bnfdoc, ls_j_1bnfe_active, gw_saida_cte.
    ENDLOOP.
  ELSEIF var_tp_alv = 2.
    LOOP AT lt_zlest0063 INTO ls_zlest0063.
      LOOP AT lt_zlest0061 INTO ls_zlest0061 WHERE bukrs      EQ ls_zlest0063-bukrs
                                               AND werks      EQ ls_zlest0063-werks
                                               AND ano_viagem EQ ls_zlest0063-ano_viagem
                                               AND nr_viagem  EQ ls_zlest0063-nr_viagem
                                               AND nome_emb   EQ ls_zlest0063-nome_emb.

        READ TABLE lt_j_1bnfdoc INTO ls_j_1bnfdoc WITH KEY  docnum  = ls_zlest0061-docnum.
        IF sy-subrc = 0.

          gw_saida_cte-nome_emb      = ls_zlest0063-nome_emb.
          gw_saida_cte-bukrs         = ls_zlest0063-bukrs.
          gw_saida_cte-werks         = ls_zlest0063-werks.
          gw_saida_cte-cl_codigo     = ls_zlest0061-cl_codigo.
          gw_saida_cte-data_chegada  = ls_zlest0061-dt_chegada.
          gw_saida_cte-data_fatura   = ls_zlest0061-dt_fatura.
          gw_saida_cte-ck_anulado    = ls_zlest0061-ck_anulado.
          gw_saida_cte-hr_prevista   = ls_zlest0061-hr_chegada.
          gw_saida_cte-docnum        = ls_zlest0061-docnum.
          gw_saida_cte-peso_descarga = ls_zlest0061-peso_chegada.
          gw_saida_cte-peso_faturado = ls_zlest0061-peso_vinculado.

          READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_zlest0063-cod_material.
          gw_saida_cte-cod_material = ls_zlest0063-cod_material.
          gw_saida_cte-maktx        = ls_makt-maktx.

          READ TABLE lt_zlest0060 INTO ls_zlest0060 WITH KEY docnum = ls_zlest0061-docnum.
          IF sy-subrc = 0.
            gw_saida_cte-nr_viagem      = ls_zlest0060-nr_viagem.
            gw_saida_cte-ano_viagem     = ls_zlest0060-ano_viagem.

            CASE ls_zlest0063-tp_class.
              WHEN: 'CO'.
                gw_saida_cte-tp_class = 'Convencional'.
              WHEN: 'R1'.
                gw_saida_cte-tp_class = 'R1-RR'.
              WHEN: 'R2'.
                gw_saida_cte-tp_class = 'R2-RR2'.
            ENDCASE.
          ENDIF.

*"// wbarbosa 16102024 US-153329
          IF ls_zlest0063-eudr EQ gc_atende_eudr.
            gw_saida_cte-ico_eudr = icon_checked.
          ELSE.
            CLEAR gw_saida_cte-ico_eudr.
          ENDIF.
*"// wbarbosa 16102024 US-153329

          READ TABLE lt_j_1bnfdoc INTO ls_j_1bnfdoc WITH KEY docnum = ls_zlest0061-docnum.
          IF ( sy-subrc EQ 0 ) AND NOT ( ls_j_1bnfdoc-nfenum IS INITIAL ).
            READ TABLE lt_j_1bnfe_active INTO ls_j_1bnfe_active WITH KEY docnum = ls_j_1bnfdoc-docnum.
            gw_saida_cte-nfenum        = ls_j_1bnfdoc-nfenum.
          ENDIF.
          APPEND gw_saida_cte TO gt_saida_cte.

        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " SELECIONAR_CTE
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_CTE
*&---------------------------------------------------------------------*
FORM criar_alv_cte .

  DATA: wl_layout   TYPE lvc_s_layo.
  CLEAR: wl_layout.

  PERFORM montar_catalog_cte USING:
      'NFENUM'       'Ct-e'            '15'   '' '' '' '' '' '' '' '' '' ,
      'CL_CODIGO'    'Cód. Cliente'    '15'   '' '' '' '' '' '' '' '' '' ,
      'PESO_FISCAL ' 'Peso Fat.'       '15'   '' '' '' '' '' '' '' '' '' ,
      'PESO_CHEGADA' 'Peso Cheg.'      '15'   '' '' '' '' '' '' '' '' ''.

  IF ( obj_custom_cte IS INITIAL ).

    CREATE OBJECT obj_custom_cte
      EXPORTING
        container_name              = 'CONTAINER_CTE'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    CREATE OBJECT obj_grid_cte
      EXPORTING
        i_parent          = obj_custom_cte
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    wl_layout-sel_mode   = 'A'.
    wl_layout-no_toolbar = 'X'.

    CALL METHOD obj_grid_cte->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        i_save                        = 'U'
      CHANGING
        it_outtab                     = gt_saida_cte[]
        it_fieldcatalog               = gt_fcat_cte[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD obj_grid_cte->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDFORM.                    " CRIAR_ALV_CTE
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_CTE
*&---------------------------------------------------------------------*
FORM montar_catalog_cte  USING  VALUE(p_fieldname)
                                   VALUE(p_desc)
                                   VALUE(p_tam)
                                   VALUE(p_no_zero)
                                   VALUE(p_hotspot)
                                   VALUE(p_cor)
                                   VALUE(p_just)
                                   VALUE(p_sum)
                                   VALUE(p_edit)
                                   VALUE(p_ref_tabname)   LIKE dd02d-tabname
                                   VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                                   VALUE(p_tabname)       LIKE dd02d-tabname.

  CLEAR: gw_fcat_cte.

  gw_fcat_cte-fieldname = p_fieldname.
  gw_fcat_cte-ref_table = p_ref_tabname..
  gw_fcat_cte-ref_field = p_ref_fieldname.
  gw_fcat_cte-tabname   = p_tabname.
  gw_fcat_cte-scrtext_l = p_desc.
  gw_fcat_cte-scrtext_m = p_desc.
  gw_fcat_cte-scrtext_s = p_desc.
  gw_fcat_cte-outputlen = p_tam.
  gw_fcat_cte-no_zero   = p_no_zero.
  gw_fcat_cte-hotspot   = p_hotspot.
  gw_fcat_cte-emphasize = p_cor.
  gw_fcat_cte-just      = p_just.
  gw_fcat_cte-do_sum    = p_sum.
  gw_fcat_cte-edit      = p_edit.

  APPEND gw_fcat_cte TO gt_fcat_cte.

ENDFORM.                    " MONTAR_CATALOG_CTE

FORM disp_cct USING p_okcode TYPE sy-ucomm.

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  DATA: lt_zlest0061 TYPE TABLE OF zlest0061,
        ls_zlest0061 TYPE zlest0061,
        lt_zlest0060 TYPE TABLE OF zlest0060,
        ls_zlest0060 TYPE zlest0060,
        lt_zlest0142 TYPE TABLE OF zlest0142,
        ls_zlest0142 TYPE zlest0142.

  DATA: v_msg     TYPE string,
        lv_okcode TYPE sy-ucomm,
        lv_erro   TYPE c.

  lv_okcode = p_okcode.

  CALL METHOD obj_grid_viagem->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF var_tp_alv NE '3'.
    MESSAGE 'Opção disponivel somente na Visão de NF-e!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF tl_rows[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  LOOP AT tl_rows INTO sl_rows.

    CLEAR: lt_zlest0060[], ls_zlest0142.

    READ TABLE gt_saida_nfe ASSIGNING FIELD-SYMBOL(<saida_nfe>) INDEX sl_rows-index.

    CHECK ( sy-subrc = 0 ) AND
          ( <saida_nfe>-vinc_tot = icon_okay ) AND
          ( <saida_nfe>-disp_cct = icon_led_yellow ).

    SELECT *
      FROM zlest0060 INTO TABLE lt_zlest0060
     WHERE chave_nfe EQ <saida_nfe>-chave_nfe.

    SORT lt_zlest0060 BY dt_chegada hr_chegada DESCENDING.

    READ TABLE lt_zlest0060 INTO ls_zlest0060 INDEX 1.

    CHECK sy-subrc = 0.

    IF ( ls_zlest0060-chave_nfe IS INITIAL ).
*      ROLLBACK WORK.
      MESSAGE 'Existem notas com chave NF-e/NF-f em branco!' TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.

    IF ( strlen( ls_zlest0060-chave_nfe ) NE 44 ) AND ( ls_zlest0060-chave_nfe(1) NE 'F' ).
*      ROLLBACK WORK.
      MESSAGE 'Existem notas com chave NF-e incompleta!' TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.

    IF ( ls_zlest0060-rm_codigo IS INITIAL ).
*      ROLLBACK WORK.
      MESSAGE |Remetente não encontrado!'| TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_zlest0060-rm_codigo
      IMPORTING
        output = ls_zlest0060-rm_codigo.

    SELECT SINGLE *
      FROM zlest0161 INTO @DATA(_0161)
     WHERE lifnr = @ls_zlest0060-rm_codigo.

    IF ( sy-subrc = 0 ).
*      ROLLBACK WORK.
      MESSAGE | Envio de notas do Remetente { ls_zlest0060-rm_codigo } não permitida( Utilizar Transação ZLES0157 )!'| TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(lwa_lfa1_reme)
     WHERE lifnr EQ @ls_zlest0060-rm_codigo.

    IF sy-subrc NE 0.
*      ROLLBACK WORK.
      MESSAGE |Cadastro fornecedor remetente: {  ls_zlest0060-rm_codigo } não encontrado!| TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.

    LOOP AT lt_zlest0060 INTO ls_zlest0060.
*      IF LS_ZLEST0060-PESO_CHEGADA <= 0.
*        ROLLBACK WORK.
*        MESSAGE | Peso de chegada ainda não informado totalmente para a chave de NF-e: { LS_ZLEST0060-CHAVE_NFE } !'| TYPE 'S'.
*        RETURN.
*      ENDIF.

      SELECT SINGLE *
        FROM zlest0061 INTO @DATA(_wl_0061)
       WHERE docnum = @ls_zlest0060-docnum.

      IF ( sy-subrc NE 0 ) OR ( ls_zlest0060-docnum IS INITIAL ).
*        ROLLBACK WORK.
        CONCATENATE 'Documento CT-e não emitido para a NF-e:' ls_zlest0060-chave_nfe INTO v_msg SEPARATED BY space.
        CONCATENATE v_msg '- Empresa:' ls_zlest0060-bukrs '/ Centro:' ls_zlest0060-werks '/ Ano: ' ls_zlest0060-ano_viagem ' / Viagem:' ls_zlest0060-nr_viagem '!' INTO v_msg SEPARATED BY space.
        MESSAGE v_msg TYPE 'S'.
        lv_erro = abap_true.
        EXIT.
      ENDIF.

      IF ( _wl_0061-dt_chegada_terminal IS INITIAL ) OR ( _wl_0061-dt_chegada_terminal EQ space ).
*        ROLLBACK WORK.
        CONCATENATE 'Chegada Terminal ainda não definada para a NF-e:' ls_zlest0060-chave_nfe INTO v_msg SEPARATED BY space.
        CONCATENATE v_msg '- Empresa:' ls_zlest0060-bukrs '/ Centro:' ls_zlest0060-werks '/ Ano: ' ls_zlest0060-ano_viagem ' / Viagem:' ls_zlest0060-nr_viagem '!' INTO v_msg SEPARATED BY space.
        MESSAGE v_msg TYPE 'S'.
        lv_erro = abap_true.
        EXIT.

      ENDIF.

    ENDLOOP.

    IF lv_erro IS NOT INITIAL.
      CLEAR lv_erro.
      CONTINUE.
    ENDIF.

    IF ls_zlest0060-ch_referencia IS NOT INITIAL.

      SELECT SINGLE *
        FROM zsdt0001 INTO @DATA(wl_zsdt0001)
       WHERE ch_referencia = @ls_zlest0060-ch_referencia.

      IF sy-subrc NE 0.
*        ROLLBACK WORK.
        MESSAGE |Romaneio: { ls_zlest0060-ch_referencia } não encontrado!| TYPE 'S'.
        CONTINUE.
*        RETURN.
      ENDIF.

    ELSE.
      CLEAR: wl_zsdt0001.
    ENDIF.

    CLEAR: ls_zlest0142.

    FREE zcl_cct_control_nf.
    CREATE OBJECT zcl_cct_control_nf.

    IF ls_zlest0060-ch_referencia IS NOT INITIAL.
      ls_zlest0142-ch_referencia    = wl_zsdt0001-ch_referencia.
      ls_zlest0142-local_descarga   = wl_zsdt0001-local_descarga.
      ls_zlest0142-cfop             = wl_zsdt0001-cfop.
    ELSE.
      ls_zlest0142-sem_romaneio   = abap_true.

      IF ls_zlest0060-chave_nfe(1) EQ 'F'. "NF Formulario.

        SELECT SINGLE *
          FROM zlest0205 INTO @DATA(lwa_zlest0205)
         WHERE chave_nfe EQ @ls_zlest0060-chave_nfe.

        IF sy-subrc EQ 0.
          ls_zlest0142-cfop  = lwa_zlest0205-cfop.
        ENDIF.

        IF ls_zlest0142-cfop IS INITIAL.
*          ROLLBACK WORK.
          MESSAGE |CFOP nota chave : { ls_zlest0060-chave_nfe } não encontrado!| TYPE 'S'.
          CONTINUE.
*          RETURN.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ls_zlest0060-chave_nfe(1) EQ 'F'. "NF Formulario.

      IF lwa_lfa1_reme-stcd1 IS NOT INITIAL.
        ls_zlest0142-cnpj_emissor   = lwa_lfa1_reme-stcd1.
      ELSE.
        ls_zlest0142-cpf_emissor    = lwa_lfa1_reme-stcd2.
      ENDIF.

      ls_zlest0142-chave_nff        = ls_zlest0060-chave_nfe.
      ls_zlest0142-numero           = ls_zlest0060-nfnum.
      ls_zlest0142-serie            = ls_zlest0060-series.
      ls_zlest0142-model            = ls_zlest0060-chave_nfe+21(2).
      ls_zlest0142-sigla_uf_emissor = 'BR-' && ls_zlest0060-chave_nfe+1(2).

      LOOP AT lt_zlest0060 INTO ls_zlest0060.
        ADD ls_zlest0060-netwr  TO ls_zlest0142-netwr.
      ENDLOOP.

    ELSE.

      ls_zlest0142-chave_nfe        = ls_zlest0060-chave_nfe.
      ls_zlest0142-cnpj_emissor     = ls_zlest0060-chave_nfe+06(14).
      ls_zlest0142-numero           = ls_zlest0060-chave_nfe+25(9).
      ls_zlest0142-serie            = ls_zlest0060-chave_nfe+22(3).
      ls_zlest0142-model            = ls_zlest0060-chave_nfe+20(2).

    ENDIF.


    ls_zlest0142-dt_emissao         = ls_zlest0060-docdat.
    ls_zlest0142-dt_chegada         = ls_zlest0060-dt_chegada.
    ls_zlest0142-hr_chegada         = ls_zlest0060-hr_chegada.
    ls_zlest0142-bukrs_rom          = ls_zlest0060-bukrs_rom.
    ls_zlest0142-branch_rom         = ls_zlest0060-branch_rom.

    "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
    PERFORM f_get_branch_ra CHANGING ls_zlest0142-bukrs_ra ls_zlest0142-branch_ra.
    CHECK ls_zlest0142-branch_ra IS NOT INITIAL.
    "ls_zlest0142-bukrs_ra           = ls_zlest0060-bukrs_rom.
    "ls_zlest0142-branch_ra          = ls_zlest0060-branch_rom.
    "LES- US 153329- ZLES0088- Ajustes EUDR- WPP <<---

    ls_zlest0142-rm_codigo          = ls_zlest0060-rm_codigo.
    ls_zlest0142-matnr              = ls_zlest0060-matnr.
    ls_zlest0142-modal              = '03'. "Aquaviário

    "Chave CT-e
    PERFORM f_atrib_chave USING ls_zlest0060-docnum
                       CHANGING ls_zlest0142.

    IF ( ls_zlest0142-chave_cte IS INITIAL ) OR
       ( strlen( ls_zlest0142-chave_cte ) NE 44 ).
*      ROLLBACK WORK.
      MESSAGE |Chave NF-e { ls_zlest0060-chave_nfe } com chave CT-e incompleta!| TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.


    ls_zlest0142-data_reg         = sy-datum.
    ls_zlest0142-hora_reg         = sy-uzeit.

    LOOP AT lt_zlest0060 INTO ls_zlest0060.
      "ADD LS_ZLEST0060-PESO_CHEGADA TO LS_ZLEST0142-PESO_CHEGADA.
      ADD ls_zlest0060-peso_fiscal  TO ls_zlest0142-peso_chegada.
      ADD ls_zlest0060-peso_fiscal  TO ls_zlest0142-peso_fiscal.
    ENDLOOP.

    zcl_cct_control_nf->atribuir_nf( i_zlest0142 = ls_zlest0142 ).
    DATA(_disponibilizada) = zcl_cct_control_nf->disp_nf_cct( ).

    IF _disponibilizada EQ abap_true.
      <saida_nfe>-disp_cct = icon_okay.
      DATA(_disp_cct) = 'X'.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'ZLES0147'
    EXCEPTIONS
      ok     = 1
      not_ok = 2.
  IF sy-subrc  EQ 1.
    IF _disp_cct IS NOT INITIAL.
      MESSAGE 'Notas selecionada(s), disponibilizada(s) para registro de CCT com sucesso!' TYPE 'S'.
*** Inicio - Rubenilson Pereira - 16.06.2022 - #54384
      IF lv_okcode NE 'REC_CARGA_CCT'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Recepção de carga CCT'
            text_question         = 'Deseja recepcionar as cargas no CCT?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'Não'
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ' '
            popup_type            = 'ICON_MESSAGE_WARNING'
          IMPORTING
            answer                = answer.

        IF answer EQ 1.
          PERFORM f_recepcao_carga.
        ENDIF.
*** Fim - Rubenilson Pereira - 16.06.2022 - #54384

        LEAVE TO SCREEN 0100.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM selecionar_nfe  USING p_saida_viagem TYPE ty_saida_viagem.

  DATA: lt_zlest0061      TYPE TABLE OF zlest0061,
        ls_zlest0061      TYPE zlest0061,

        lt_zlest0060      TYPE TABLE OF zlest0060,
        ls_zlest0060      TYPE zlest0060,

        lt_zlest0073      TYPE TABLE OF zlest0073,
        ls_zlest0073      TYPE zlest0073,

        lt_zlest0063      TYPE TABLE OF zlest0063,
        ls_zlest0063      TYPE zlest0063,

        lt_zlest0142      TYPE TABLE OF zlest0142,
        ls_zlest0142      TYPE zlest0142,

        lt_zsdt0001       TYPE TABLE OF zsdt0001,
        ls_zsdt0001       TYPE zsdt0001,

        lt_zfiwrt0008_aux TYPE TABLE OF zfiwrt0008,

        lt_zfiwrt0008     TYPE TABLE OF zfiwrt0008,
        ls_zfiwrt0008     TYPE zfiwrt0008,

        lt_zib_chv        TYPE TABLE OF zib_contabil_chv,
        ls_zib_chv        TYPE zib_contabil_chv.

  DATA: lt_j_1bnfdoc TYPE TABLE OF j_1bnfdoc,
        ls_j_1bnfdoc TYPE j_1bnfdoc.

  DATA: lt_j_1bnfe_active TYPE TABLE OF j_1bnfe_active,
        ls_j_1bnfe_active TYPE j_1bnfe_active.

  DATA: lt_zlest0060_aux TYPE TABLE OF zlest0060.

  DATA: tg_lfa1 TYPE TABLE OF lfa1 WITH HEADER LINE.
  DATA: tg_makt TYPE TABLE OF makt WITH HEADER LINE.

  CLEAR: gw_saida_nfe, gt_saida_nfe[], lt_zlest0063[], lt_zfiwrt0008[], lt_zib_chv[].

  SELECT *
    FROM zlest0061 INTO TABLE lt_zlest0061
   WHERE bukrs      EQ wp_tela_0100-wp_bukrs
     AND werks      EQ wp_tela_0100-wp_werks
     AND ano_viagem EQ wp_tela_0100-wp_ano
     AND nr_viagem  EQ wp_tela_0100-wp_viagem.

  CHECK lt_zlest0061[] IS NOT INITIAL.

  SELECT *
    FROM zlest0060 INTO TABLE lt_zlest0060
     FOR ALL ENTRIES IN lt_zlest0061
   WHERE bukrs         EQ lt_zlest0061-bukrs
     AND werks         EQ lt_zlest0061-werks
     AND nr_viagem     EQ lt_zlest0061-nr_viagem
     AND ano_viagem    EQ lt_zlest0061-ano_viagem
     AND id_frete_aqua EQ lt_zlest0061-id_frete_aqua.

  SELECT *
    FROM zlest0063 INTO TABLE lt_zlest0063
   WHERE bukrs      EQ wp_tela_0100-wp_bukrs
     AND werks      EQ wp_tela_0100-wp_werks
     AND nr_viagem  EQ wp_tela_0100-wp_viagem
     AND ano_viagem EQ wp_tela_0100-wp_ano
     AND embarcacao EQ 'B'.

  CHECK lt_zlest0060[] IS NOT INITIAL.

  SELECT *
    FROM zlest0073 INTO TABLE lt_zlest0073
     FOR ALL ENTRIES IN lt_zlest0060
   WHERE chave_nfe  EQ lt_zlest0060-chave_nfe.

  SELECT *
    FROM zlest0142 INTO TABLE lt_zlest0142
     FOR ALL ENTRIES IN lt_zlest0060
   WHERE chave_nfe  EQ lt_zlest0060-chave_nfe.

  SELECT *
   FROM zlest0142 APPENDING CORRESPONDING FIELDS OF TABLE lt_zlest0142          "Ajuste realizado - IR064882 - aoenning. 02/07/2021
    FOR ALL ENTRIES IN lt_zlest0060
  WHERE chave_nff  EQ lt_zlest0060-chave_nfe.
*** Inicio - Rubenilson Pereira - 16.06.2022 - #54384
  IF lt_zlest0142  IS NOT INITIAL.
    DATA(lt_zlest0142_aux) = lt_zlest0142.

    SORT lt_zlest0142_aux BY id_recepcao.

    DELETE ADJACENT DUPLICATES FROM lt_zlest0142_aux COMPARING id_recepcao.

    SELECT id_recepcao
      FROM zlest0146
      INTO TABLE @DATA(lt_zlest0146)
      FOR ALL ENTRIES IN @lt_zlest0142_aux
      WHERE id_recepcao = @lt_zlest0142_aux-id_recepcao
        AND cancel      = @space.
    IF sy-subrc IS INITIAL.
      SORT lt_zlest0146 BY id_recepcao.
    ENDIF.

  ENDIF.
*** Fim- Rubenilson Pereira - 16.06.2022 - #54384

  SELECT *
    FROM zsdt0001 INTO TABLE lt_zsdt0001
     FOR ALL ENTRIES IN lt_zlest0060
   WHERE ch_referencia  EQ lt_zlest0060-ch_referencia.

*-IR220444-04.02.2025-#165558-JT-inicio
* CHECK lt_zsdt0001[] IS NOT INITIAL.
*-IR220444-04.02.2025-#165558-JT-fim

  SELECT *
    FROM j_1bnfdoc INTO TABLE lt_j_1bnfdoc
     FOR ALL ENTRIES IN lt_zlest0061
   WHERE docnum EQ lt_zlest0061-docnum
     AND nfe    EQ 'X'
     AND cancel NE 'X'.

  CHECK NOT lt_j_1bnfdoc[] IS INITIAL.

  SELECT *
    FROM j_1bnfe_active INTO TABLE lt_j_1bnfe_active
     FOR ALL ENTRIES IN lt_j_1bnfdoc
   WHERE docnum EQ lt_j_1bnfdoc-docnum
     AND cancel NE 'X'.

  CHECK NOT lt_j_1bnfe_active[] IS INITIAL.

  SELECT *
    FROM lfa1 INTO TABLE tg_lfa1
     FOR ALL ENTRIES IN lt_zlest0060
   WHERE lifnr  EQ lt_zlest0060-rm_codigo.

  SELECT *
    FROM makt INTO TABLE tg_makt
     FOR ALL ENTRIES IN lt_zlest0060
   WHERE matnr EQ lt_zlest0060-matnr
     AND spras EQ sy-langu.

  lt_zlest0060_aux[] = lt_zlest0060[].
  DELETE lt_zlest0060_aux WHERE docnum_rem IS INITIAL.

  IF lt_zlest0060_aux[] IS NOT INITIAL.
    SELECT *
      FROM zfiwrt0008 INTO TABLE lt_zfiwrt0008
       FOR ALL ENTRIES IN lt_zlest0060_aux
     WHERE docnum_saida EQ lt_zlest0060_aux-docnum_rem.
  ENDIF.

  lt_zfiwrt0008_aux[] = lt_zfiwrt0008[].
  DELETE lt_zfiwrt0008_aux WHERE obj_key IS INITIAL.

  IF lt_zfiwrt0008_aux[] IS NOT INITIAL.
    SELECT *
      FROM zib_contabil_chv INTO TABLE lt_zib_chv
       FOR ALL ENTRIES IN lt_zfiwrt0008_aux
     WHERE obj_key EQ lt_zfiwrt0008_aux-obj_key.
  ENDIF.

  LOOP AT lt_zlest0061 INTO ls_zlest0061.

    READ TABLE lt_j_1bnfdoc INTO ls_j_1bnfdoc WITH KEY docnum = ls_zlest0061-docnum.
    CHECK ( sy-subrc EQ 0 ) AND ( ls_j_1bnfdoc-nfenum IS NOT INITIAL ).

    READ TABLE lt_j_1bnfe_active INTO ls_j_1bnfe_active WITH KEY docnum = ls_j_1bnfdoc-docnum.
    CHECK sy-subrc = 0.

    LOOP AT lt_zlest0060 INTO ls_zlest0060 WHERE docnum = ls_zlest0061-docnum.

      CLEAR: gw_saida_nfe, ls_zlest0073, ls_zsdt0001, ls_zlest0142.

      READ TABLE lt_zsdt0001  INTO ls_zsdt0001  WITH KEY ch_referencia = ls_zlest0060-ch_referencia.
      READ TABLE lt_zlest0073 INTO ls_zlest0073 WITH KEY chave_nfe   = ls_zlest0060-chave_nfe.
      READ TABLE lt_zlest0142 INTO ls_zlest0142 WITH KEY chave_nfe   = ls_zlest0060-chave_nfe.
      IF sy-subrc NE 0.
        CLEAR: ls_zlest0142.
        READ TABLE lt_zlest0142 INTO ls_zlest0142 WITH KEY chave_nff   = ls_zlest0060-chave_nfe.
      ENDIF.
*** Inicio - Rubenilson Pereira - 16.06.2022 - #54384
      IF ls_zlest0142-id_recepcao IS NOT INITIAL.
        READ TABLE lt_zlest0146 TRANSPORTING NO FIELDS
        WITH KEY id_recepcao = ls_zlest0142-id_recepcao
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          gw_saida_nfe-recep_cct = icon_okay.

        ELSE.

          gw_saida_nfe-recep_cct =   icon_led_yellow.

        ENDIF.

      ELSE.

        gw_saida_nfe-recep_cct = icon_led_yellow.

      ENDIF.
*** Fim - Rubenilson Pereira - 16.06.2022 - #54384

      gw_saida_nfe-nfnum          = ls_zlest0060-nfnum.
      gw_saida_nfe-docdat         = ls_zlest0060-docdat.
      gw_saida_nfe-chave_nfe      = ls_zlest0060-chave_nfe.
      gw_saida_nfe-dt_chegada     = ls_zlest0060-dt_chegada.
      gw_saida_nfe-hr_chegada     = ls_zlest0060-hr_chegada.
      gw_saida_nfe-peso_chegada   = ls_zlest0060-peso_chegada.
      gw_saida_nfe-peso_fiscal    = ls_zlest0060-peso_fiscal.
      gw_saida_nfe-ch_referencia  = ls_zlest0060-ch_referencia.
      gw_saida_nfe-nome_emb       = ls_zlest0060-nome_emb.
      gw_saida_nfe-num_cte        = ls_j_1bnfdoc-nfenum.

      CASE ls_zlest0061-tp_class.
        WHEN 'CO'.
          gw_saida_nfe-tp_transgenia = 'Convencional'.
        WHEN 'R1'.
          gw_saida_nfe-tp_transgenia = 'R1-RR'.
        WHEN 'R2'.
          gw_saida_nfe-tp_transgenia = 'R2-RR2'.
      ENDCASE.

*"// wbarbosa 16102024 US-153329
      IF ls_zlest0061-eudr EQ gc_atende_eudr.
        gw_saida_nfe-ico_eudr = icon_checked.
      ELSE.
        CLEAR gw_saida_nfe-ico_eudr.
      ENDIF.
*"// wbarbosa 16102024 US-153329

      gw_saida_nfe-rm_codigo      = ls_zlest0060-rm_codigo.
      READ TABLE tg_lfa1 WITH KEY lifnr = gw_saida_nfe-rm_codigo.
      IF sy-subrc = 0.
        gw_saida_nfe-name1        = tg_lfa1-name1.
      ENDIF.

      gw_saida_nfe-matnr          = ls_zlest0060-matnr.
      READ TABLE tg_makt WITH KEY matnr = gw_saida_nfe-matnr.
      IF sy-subrc = 0.
        gw_saida_nfe-maktx        = tg_makt-maktx.
      ENDIF.

*      READ TABLE LT_ZLEST0063 INTO LS_ZLEST0063 WITH KEY BUKRS       =  LS_ZLEST0060-BUKRS
*                                                         WERKS       =  LS_ZLEST0060-WERKS
*                                                         NR_VIAGEM   =  LS_ZLEST0060-NR_VIAGEM
*                                                         ANO_VIAGEM  =  LS_ZLEST0060-ANO_VIAGEM.
*      IF SY-SUBRC EQ 0.
*        CASE LS_ZLEST0063-TP_CLASS.
*          WHEN 'CO'.
*            GW_SAIDA_NFE-TP_TRANSGENIA = 'Convencional'.
*          WHEN 'R1'.
*            GW_SAIDA_NFE-TP_TRANSGENIA = 'R1-RR'.
*          WHEN 'R2'.
*            GW_SAIDA_NFE-TP_TRANSGENIA = 'R2-RR2'.
*        ENDCASE.
*      ENDIF.

*"// wbarbosa 16102024 US-153329
      IF ls_zlest0060-eudr EQ gc_atende_eudr.
        gw_saida_nfe-ico_eudr = icon_checked.
      ELSE.
        CLEAR gw_saida_nfe-ico_eudr.
      ENDIF.
*"// wbarbosa 16102024 US-153329

      DATA(_ctes_autorizados) = ''.
      PERFORM f_check_ctes_autorizados USING ls_zlest0060-chave_nfe
                                    CHANGING _ctes_autorizados.

      IF ( ls_zlest0073-peso_origem    = ls_zlest0073-peso_vinculado ) AND
         ( ls_zlest0073-peso_vinculado > 0 ) AND
         ( _ctes_autorizados IS NOT INITIAL ) . "Todos os CT-e's Autorizados
        gw_saida_nfe-vinc_tot       = icon_okay.
      ELSE.
        gw_saida_nfe-vinc_tot       = icon_led_yellow.

        IF ( gw_saida_nfe-ch_referencia IS NOT INITIAL ) AND ( ls_zlest0073-peso_vinculado > 0 ).
          SELECT SINGLE *
            FROM zlest0158 INTO @DATA(_wl_0158)
           WHERE ch_referencia EQ @gw_saida_nfe-ch_referencia.

          IF sy-subrc EQ 0. "Romaneio já teve baixa
            gw_saida_nfe-vinc_tot       = icon_okay.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_zlest0142 IS NOT INITIAL.
        gw_saida_nfe-disp_cct       = icon_okay.
      ELSE.
        gw_saida_nfe-disp_cct       = icon_led_yellow.
      ENDIF.

      IF wp_tela_0100-operacao IS NOT INITIAL.
        CHECK ls_zlest0060-operacao EQ wp_tela_0100-operacao.
      ENDIF.
      "Remessa para Industrialização
      IF ( ls_zlest0060-docnum_rem IS NOT INITIAL ) AND ( ls_zlest0060-operacao EQ 'RI').

        gw_saida_nfe-disp_cct = icon_led_inactive.

        READ TABLE lt_zfiwrt0008 INTO ls_zfiwrt0008 WITH KEY docnum_saida    = ls_zlest0060-docnum_rem
                                                             docs_estornados = abap_false
                                                             loekz           = abap_false.
        IF ( sy-subrc EQ 0 ).
          gw_saida_nfe-seq_lcto_ind = ls_zfiwrt0008-seq_lcto.

          gw_saida_nfe-docnum_ind   = ls_zfiwrt0008-docnum.
          gw_saida_nfe-nfenum_ind   = ls_zfiwrt0008-nfenum.

          IF ls_zfiwrt0008-docnum IS INITIAL.
            gw_saida_nfe-nfenum_ind  = icon_execute_object.
          ENDIF.

          READ TABLE lt_zib_chv INTO ls_zib_chv WITH KEY obj_key = ls_zfiwrt0008-obj_key.
          IF ( sy-subrc EQ 0 ) AND ( ls_zfiwrt0008-obj_key IS NOT INITIAL ) AND ( ls_zib_chv-belnr IS NOT INITIAL ).
            gw_saida_nfe-belnr_ind = ls_zib_chv-belnr.
          ENDIF.

        ENDIF.
      ENDIF.

      APPEND gw_saida_nfe TO gt_saida_nfe.
    ENDLOOP.
  ENDLOOP.


ENDFORM.

FORM remover_cct.

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  DATA: lt_zlest0061 TYPE TABLE OF zlest0061,
        ls_zlest0061 TYPE zlest0061,
        lt_zlest0060 TYPE TABLE OF zlest0060,
        ls_zlest0060 TYPE zlest0060,
        lt_zlest0142 TYPE TABLE OF zlest0142,
        ls_zlest0142 TYPE zlest0142.

  CALL METHOD obj_grid_viagem->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF var_tp_alv NE '3'.
    MESSAGE 'Opção disponivel somente na Visão de NF-e!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF tl_rows[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  DATA(_rem_cct) = abap_false.

  LOOP AT tl_rows INTO sl_rows.

    CLEAR: lt_zlest0060[], ls_zlest0142.

    READ TABLE gt_saida_nfe ASSIGNING FIELD-SYMBOL(<saida_nfe>) INDEX sl_rows-index.

    CHECK ( sy-subrc = 0 ) AND ( <saida_nfe>-disp_cct = icon_okay ) AND ( <saida_nfe>-chave_nfe  IS NOT INITIAL ).

    IF ( <saida_nfe>-rm_codigo IS INITIAL ).
      ROLLBACK WORK.
      MESSAGE |Remetente não encontrado!'| TYPE 'S'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <saida_nfe>-rm_codigo
      IMPORTING
        output = <saida_nfe>-rm_codigo.

    SELECT SINGLE *
      FROM zlest0161 INTO @DATA(_0161)
     WHERE lifnr = @<saida_nfe>-rm_codigo.

    IF ( sy-subrc = 0 ).
      ROLLBACK WORK.
      MESSAGE | Alteração de notas do Remetente { <saida_nfe>-rm_codigo } não permitida( Utilizar Transação ZLES0157 )!'| TYPE 'S'.
      RETURN.
    ENDIF.

    IF <saida_nfe>-chave_nfe(1) EQ 'F'.

      SELECT SINGLE *
        FROM zlest0142 INTO ls_zlest0142
       WHERE chave_nff EQ <saida_nfe>-chave_nfe.

    ELSE.

      SELECT SINGLE *
        FROM zlest0142 INTO ls_zlest0142
       WHERE chave_nfe EQ <saida_nfe>-chave_nfe.

    ENDIF.

    IF ( sy-subrc EQ 0 ).

      FREE zcl_cct_control_nf.
      CREATE OBJECT zcl_cct_control_nf.
      DATA(_removida) = zcl_cct_control_nf->remover_nf_cct( i_zlest0142 = ls_zlest0142 ).
      IF _removida EQ abap_true.
        _rem_cct = abap_true.
        <saida_nfe>-disp_cct = icon_led_yellow.
      ENDIF.

    ELSE.

      MESSAGE |Não encontrado registro CCT (Tabela ZLEST0142), para a nota selecionada { <saida_nfe>-chave_nfe } !| TYPE 'S'.

*      UPDATE ZLEST0060 SET ST_CCT = SPACE " Não utilizado
*       WHERE CHAVE_NFE = <SAIDA_NFE>-CHAVE_NFE.
*
*      UPDATE ZSDT0001 SET ST_CCT = SPACE " Não utilizado
*       WHERE CH_REFERENCIA = <SAIDA_NFE>-CH_REFERENCIA.
*
*      _REM_CCT = ABAP_TRUE.
*      <SAIDA_NFE>-DISP_CCT = ICON_LED_YELLOW.

    ENDIF.

  ENDLOOP.

  IF _rem_cct IS NOT INITIAL.
    MESSAGE 'Notas selecionada(s), removida(s) do registro de CCT com sucesso!' TYPE 'S'.
    LEAVE TO SCREEN 0100.
  ENDIF.

ENDFORM.

FORM f_atrib_chave  USING p_docnum    TYPE zlest0060-docnum
                 CHANGING p_zlest0142 TYPE zlest0142.

  CHECK p_docnum IS NOT INITIAL.

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(_wl_active)
   WHERE docnum EQ @p_docnum.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = _wl_active-serie
    IMPORTING
      output = _wl_active-serie.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = _wl_active-nfnum9
    IMPORTING
      output = _wl_active-nfnum9.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = _wl_active-docnum9
    IMPORTING
      output = _wl_active-docnum9.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = _wl_active-stcd1
    IMPORTING
      output = _wl_active-stcd1.

  CONCATENATE _wl_active-regio
              _wl_active-nfyear
              _wl_active-nfmonth
              _wl_active-stcd1
              _wl_active-model
              _wl_active-serie
              _wl_active-nfnum9
              _wl_active-docnum9
              _wl_active-cdv INTO p_zlest0142-chave_cte.

  p_zlest0142-cnpj_transp =  _wl_active-stcd1.

ENDFORM.

FORM f_check_ctes_autorizados  USING p_chave_nfe TYPE zlest0073-chave_nfe
                            CHANGING p_ctes_autorizados.

  DATA: tg_0060   TYPE TABLE OF zlest0060 WITH HEADER LINE,
        tg_0061   TYPE TABLE OF zlest0061 WITH HEADER LINE,
        wl_0073   TYPE zlest0073,
        wl_0104   TYPE zlest0104,
        vl_docnum TYPE zlest0061-docnum.

  p_ctes_autorizados = abap_false.

  CHECK p_chave_nfe IS NOT INITIAL.

  SELECT SINGLE *
    FROM zlest0073 INTO wl_0073
   WHERE chave_nfe = p_chave_nfe.

  CHECK ( sy-subrc = 0 ) AND ( wl_0073-peso_origem > 0 ).

  CHECK ( wl_0073-peso_origem - wl_0073-peso_vinculado ) <= 0.

  SELECT *
    FROM zlest0060 INTO TABLE tg_0060
   WHERE chave_nfe EQ p_chave_nfe.

  CHECK tg_0060[] IS NOT INITIAL.

  LOOP AT tg_0060.

    SELECT SINGLE *
      FROM zlest0061 INTO tg_0061
     WHERE docnum = tg_0060-docnum.

    IF ( sy-subrc NE 0 ) OR ( tg_0060-docnum IS INITIAL ).
      RETURN.
    ENDIF.

    "Verificar se Documento já foi autorizado
    SELECT SINGLE docnum
      FROM j_1bnfe_active INTO vl_docnum
     WHERE docnum     = tg_0061-docnum
       AND cancel     = ''
       AND docsta     = '1'.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

  ENDLOOP.

  p_ctes_autorizados = abap_true.

ENDFORM.

FORM f_inf_dt_sai_comboio .

  CALL SCREEN 0101 STARTING AT 05 05.

  PERFORM: preencher_campos.

ENDFORM.

MODULE user_command_0101 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF wp_tela_0100-dt_sai_comboio IS INITIAL.
        MESSAGE 'Informar Data Saída Comboio!'  TYPE 'S'.
        EXIT.
      ENDIF.

      IF wp_tela_0100-hr_sai_comboio IS INITIAL.
        MESSAGE 'Informar Hora Saída Comboio!'  TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM zlest0068 INTO @DATA(_wl_0068)
       WHERE bukrs       = @wp_tela_0100-wp_bukrs
         AND werks       = @wp_tela_0100-wp_werks
         AND ano_viagem  = @wp_tela_0100-wp_ano
         AND nr_viagem   = @wp_tela_0100-wp_viagem.

      IF sy-subrc NE 0.
        MESSAGE |Registro Viagem Empresa: { wp_tela_0100-wp_bukrs } / Centro: { wp_tela_0100-wp_werks } / Ano: { wp_tela_0100-wp_ano } / Viagem: { wp_tela_0100-wp_viagem } não encontrado!|  TYPE 'S'.
        EXIT.
      ENDIF.

      _wl_0068-dt_saida_comboio = wp_tela_0100-dt_sai_comboio.
      _wl_0068-hr_saida_comboio = wp_tela_0100-hr_sai_comboio.

      MODIFY zlest0068 FROM _wl_0068.

      MESSAGE 'Informações salvas com sucesso!'  TYPE 'S'.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE status_0101 OUTPUT.
  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'T0101'.
ENDMODULE.

FORM f_lcto_ent_ind_znfw.

  DATA: wl_zfiwrt0008 TYPE zfiwrt0008,
        wl_zfiwrt0009 TYPE zfiwrt0009,
        wl_zlest0104  TYPE zlest0104.

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  DATA: lt_zlest0061 TYPE TABLE OF zlest0061,
        ls_zlest0061 TYPE zlest0061,
        lt_zlest0060 TYPE TABLE OF zlest0060,
        ls_zlest0060 TYPE zlest0060,
        lt_zlest0142 TYPE TABLE OF zlest0142,
        ls_zlest0142 TYPE zlest0142.

  DATA: v_msg   TYPE string,
        lv_erro TYPE c.

  CALL METHOD obj_grid_viagem->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF var_tp_alv NE '3'.
    MESSAGE 'Opção disponivel somente na Visão de NF-e!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF tl_rows[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  DATA(_lcto_gerados) = abap_false.

  LOOP AT tl_rows INTO sl_rows.

    CLEAR: lt_zlest0060[], ls_zlest0142.

    READ TABLE gt_saida_nfe ASSIGNING FIELD-SYMBOL(<saida_nfe>) INDEX sl_rows-index.

    CHECK ( sy-subrc = 0 ) AND
          ( <saida_nfe>-vinc_tot      EQ icon_okay ) AND
          ( <saida_nfe>-seq_lcto_ind  IS INITIAL   ).

    SELECT *
      FROM zlest0060 INTO TABLE lt_zlest0060
     WHERE chave_nfe EQ <saida_nfe>-chave_nfe.

    SORT lt_zlest0060 BY dt_chegada hr_chegada DESCENDING.

    READ TABLE lt_zlest0060 INTO ls_zlest0060 INDEX 1.

    CHECK sy-subrc = 0.

    IF ( ls_zlest0060-operacao NE 'RI' ). "Remessa Industrialização
*      ROLLBACK WORK.
      MESSAGE |Chave NF-e: { ls_zlest0060-chave_nfe } não é uma remessa de industrialização!| TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.

    IF ( ls_zlest0060-chave_nfe IS INITIAL ).
*      ROLLBACK WORK.
      MESSAGE 'Existem notas com chave NF-e/NF-f em branco!' TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.

    IF ( strlen( ls_zlest0060-chave_nfe ) NE 44 ) AND ( ls_zlest0060-chave_nfe(1) NE 'F' ).
*      ROLLBACK WORK.
      MESSAGE 'Existem notas com chave NF-e incompleta!' TYPE 'S'.
      CONTINUE.
*      RETURN.
    ENDIF.

    LOOP AT lt_zlest0060 INTO ls_zlest0060.
      SELECT SINGLE *
        FROM zlest0061 INTO @DATA(_wl_0061)
       WHERE docnum = @ls_zlest0060-docnum.

      IF ( sy-subrc NE 0 ) OR ( ls_zlest0060-docnum IS INITIAL ).
*        ROLLBACK WORK.
        CONCATENATE 'Documento CT-e não emitido para a NF-e:' ls_zlest0060-chave_nfe INTO v_msg SEPARATED BY space.
        CONCATENATE v_msg '- Empresa:' ls_zlest0060-bukrs '/ Centro:' ls_zlest0060-werks '/ Ano: ' ls_zlest0060-ano_viagem ' / Viagem:' ls_zlest0060-nr_viagem '!' INTO v_msg SEPARATED BY space.
        MESSAGE v_msg TYPE 'S'.

        lv_erro = abap_true.
        EXIT.
      ENDIF.

      IF ( _wl_0061-dt_chegada_terminal IS INITIAL ) OR ( _wl_0061-dt_chegada_terminal EQ space ).
*        ROLLBACK WORK.
        CONCATENATE 'Chegada Terminal ainda não definada para a NF-e:' ls_zlest0060-chave_nfe INTO v_msg SEPARATED BY space.
        CONCATENATE v_msg '- Empresa:' ls_zlest0060-bukrs '/ Centro:' ls_zlest0060-werks '/ Ano: ' ls_zlest0060-ano_viagem ' / Viagem:' ls_zlest0060-nr_viagem '!' INTO v_msg SEPARATED BY space.
        MESSAGE v_msg TYPE 'S'.

        lv_erro = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_erro IS NOT INITIAL.
      CLEAR lv_erro.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(wl_zsdt0001)
     WHERE ch_referencia = @ls_zlest0060-ch_referencia.

    IF sy-subrc NE 0.
*      ROLLBACK WORK.
      MESSAGE |Romaneio: { ls_zlest0060-ch_referencia } não encontrado!| TYPE 'S'.
*      RETURN.
      CONTINUE.
    ENDIF.

    SELECT SINGLE * FROM zlest0104 INTO wl_zlest0104 WHERE emissor = ls_zlest0060-werks.
    IF ( sy-subrc NE 0 ).
*      ROLLBACK WORK.
      MESSAGE |Não parametrizado Centro Emissor: { ls_zlest0060-werks }! Transação ZLES0111!| TYPE 'S'.
*      RETURN.
      CONTINUE.
    ENDIF.

    IF ( wl_zlest0104-opr_znfw_ent_ind IS INITIAL ).
*      ROLLBACK WORK.
      MESSAGE |Não parametrizado operação para o Centro Emissor: { ls_zlest0060-werks }! Transação ZLES0111!| TYPE 'S'.
*      RETURN.
      CONTINUE.
    ENDIF.

*    IF ( WL_ZLEST0104-LGORT_ZNFW_ENT_IND IS INITIAL ).
*      ROLLBACK WORK.
*      MESSAGE |Não parametrizado depósito entrada para o Centro Emissor: { LS_ZLEST0060-WERKS }! Transação ZLES0111!| TYPE 'S'.
*      RETURN.
*    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_zlest0060-rm_codigo
      IMPORTING
        output = ls_zlest0060-rm_codigo.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_zlest0060-dt_codigo
      IMPORTING
        output = ls_zlest0060-dt_codigo.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(wl_lfa1_dt)
     WHERE lifnr EQ @ls_zlest0060-dt_codigo.

    IF ( sy-subrc NE 0 ) OR ( wl_lfa1_dt-ktokk NE 'ZFIC' ).
*      ROLLBACK WORK.
      MESSAGE |Destinatário: { ls_zlest0060-dt_codigo } não é Intercompany!| TYPE 'S'.
*      RETURN.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_dest)
     WHERE branch EQ @wl_lfa1_dt-lifnr+6(4).

    CHECK sy-subrc EQ 0.

    IF ( sy-subrc NE 0 ).
*      ROLLBACK WORK.
      MESSAGE |Filial: { ls_zlest0060-dt_codigo } não encontrada!| TYPE 'S'.
*      RETURN.
      CONTINUE.
    ENDIF.

    IF ls_zlest0060-doc_rem IS INITIAL.
*      ROLLBACK WORK.
      MESSAGE |Chave: { ls_zlest0060-chave_nfe } não possui documento de remessa!| TYPE 'S'.
*      RETURN.
      CONTINUE.
    ENDIF.

    IF ls_zlest0060-docnum_rem IS INITIAL.
*      ROLLBACK WORK.
      MESSAGE |Chave: { ls_zlest0060-chave_nfe } não possui documento de fiscal da remessa!| TYPE 'S'.
*      RETURN.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(wl_zfiwrt0008_rem)
     WHERE docnum_saida    EQ @ls_zlest0060-docnum_rem
       AND docs_estornados EQ @abap_false
       AND loekz           EQ @abap_false.

    IF sy-subrc EQ 0.
*      ROLLBACK WORK.
      MESSAGE |Lançamento de entrada ZNFW já gerada para a Chave: { ls_zlest0060-chave_nfe }!| TYPE 'S'.
*      RETURN.
      CONTINUE.
    ENDIF.


*--------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zlest0104-opr_znfw_ent_ind.
    wl_zfiwrt0008-bukrs           = wl_branch_dest-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_dest-branch.
    wl_zfiwrt0008-parid           = ls_zlest0060-rm_codigo.
    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = ls_zlest0060-docdat.
    wl_zfiwrt0008-nfenum          = ls_zlest0060-nfnum.
    wl_zfiwrt0008-series          = ls_zlest0060-series.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-docnum_saida    = ls_zlest0060-docnum_rem.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = ls_zlest0060-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_dest-branch.
    "WL_ZFIWRT0009-LGORT  = WL_ZLEST0104-LGORT_ZNFW_ENT_IND.

    LOOP AT lt_zlest0060 INTO ls_zlest0060.
      ADD ls_zlest0060-peso_fiscal TO wl_zfiwrt0009-menge.
      ADD ls_zlest0060-netwr       TO wl_zfiwrt0009-netwr.
    ENDLOOP.

    IF wl_zfiwrt0009-menge > 0.
      wl_zfiwrt0009-netpr  = wl_zfiwrt0009-netwr / wl_zfiwrt0009-menge.
    ENDIF.

    SELECT SINGLE *
      FROM mara INTO @DATA(wl_mara)
     WHERE matnr EQ @ls_zlest0060-matnr.

    IF ( sy-subrc EQ 0 ).
      wl_zfiwrt0009-meins = wl_mara-meins.

      IF ( wl_mara-xchpf = abap_true ).
        wl_zfiwrt0009-charg = ls_zlest0060-safra.
      ENDIF.
    ENDIF.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    TRY.
        zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                                   )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                                   )->add_item( i_item = wl_zfiwrt0009 ).

        zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_gerado) ).

        IF _seq_lcto_gerado IS NOT INITIAL.
          MESSAGE |Lançamento { _seq_lcto_gerado } gerado com sucesso!| TYPE 'S'.

          <saida_nfe>-seq_lcto_ind   = _seq_lcto_gerado.

          CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
            EXPORTING
              i_seq_lcto = _seq_lcto_gerado.

          _lcto_gerados = abap_true.

        ELSE.
          MESSAGE |Houve um erro ao gravar o lançamento!| TYPE 'S'.
        ENDIF.

      CATCH zcx_nf_writer INTO DATA(zcx_nf_writer).
        zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

  ENDLOOP.

  IF _lcto_gerados = abap_true.
    MESSAGE 'Entrada(s) gerada(s) para a(s) Nota(s) selecionada(s)!' TYPE 'S'.
    LEAVE TO SCREEN 0100.
  ENDIF.


ENDFORM.


FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CHECAR_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_SAIDA_VIAGEM_COD_MATERIAL  text
*----------------------------------------------------------------------*
FORM fm_checar_material  USING p_material.

  FREE r_matnr.

  SELECT SINGLE matkl
    FROM mara
    INTO @DATA(vl_matkl)
    WHERE matnr EQ @p_material.

  CHECK sy-subrc IS INITIAL.

  vl_matkl = |{ vl_matkl ALPHA = IN }|.

  SELECT COUNT(*)
    FROM setleaf
   WHERE setname EQ 'MAGGI_ZLES0077_GR_MAT'
    AND valfrom EQ @vl_matkl.

  CHECK sy-subrc IS NOT INITIAL.

  APPEND VALUE #( sign   = 'I'
                  option = 'EQ'
                  low    = gw_saida_viagem-cod_material
                ) TO r_matnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTORIZACAO
*&---------------------------------------------------------------------*
FORM check_autorizacao  CHANGING vl_aut.
  AUTHORITY-CHECK OBJECT 'ZLES0088'
  ID 'ZACAO_0002' FIELD '01'.
  vl_aut = sy-subrc.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_365
*&---------------------------------------------------------------------*
FORM f_valida_365  USING  lv_value          TYPE lvc_value
                          ls_good-fieldname TYPE lvc_s_modi-fieldname
                          gw_saida_viagem   TYPE ty_saida_viagem
                          vl_status         TYPE sy-subrc.

  CHECK lv_value IS NOT INITIAL.

  DATA: vl_datediff TYPE  p,
        vl_timediff TYPE  p,
        vl_earliest TYPE  c.

  DATA: vl_date TYPE d,
        vl_hour TYPE sy-uzeit.

  vl_date = lv_value.
  vl_hour = '00:01:00'.

  IF ls_good-fieldname = 'DT_DESCARGA_INI'.

    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
      EXPORTING
        date1            = vl_date
        time1            = vl_hour
        date2            = gw_saida_viagem-data_chegada
        time2            = vl_hour
      IMPORTING
        datediff         = vl_datediff
        timediff         = vl_timediff
        earliest         = vl_earliest
      EXCEPTIONS
        invalid_datetime = 1
        OTHERS           = 2.
    IF sy-subrc = 0 AND vl_datediff > 365.
      vl_status = 4.
      RETURN.
    ENDIF.
  ENDIF.

  IF ls_good-fieldname = 'DT_DESCARGA_FIM'.

    CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
      EXPORTING
        date1            = vl_date
        time1            = vl_hour "<fs_saida>-hr_descarga_fim
        date2            = gw_saida_viagem-data_chegada
        time2            = vl_hour
      IMPORTING
        datediff         = vl_datediff
        timediff         = vl_timediff
        earliest         = vl_earliest
      EXCEPTIONS
        invalid_datetime = 1
        OTHERS           = 2.
    IF sy-subrc = 0 AND vl_datediff > 365.
      vl_status = 4.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_DT_SISTEMA
*&---------------------------------------------------------------------*

FORM f_valida_dt_sistema  USING    lv_value          TYPE lvc_value
                                   ls_good-fieldname TYPE lvc_s_modi-fieldname
                                   gw_saida_viagem   TYPE ty_saida_viagem
                                   vl_status         TYPE sy-subrc.

  CHECK lv_value IS NOT INITIAL.

  DATA: vl_dt_inicio TYPE timestamp,
        vl_dt_fim    TYPE timestamp,
        vl_date      TYPE d,
        vl_hour      TYPE sy-uzeit.

  CLEAR: vl_dt_inicio,
            vl_dt_fim.

*/// data da descarga deve ser menor que a data do sistema
  IF ls_good-fieldname = 'DT_DESCARGA_FIM'.
    IF gw_saida_viagem-hr_descarga_fim IS INITIAL.
      CONVERT DATE lv_value TIME sy-uzeit INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE lv_value TIME gw_saida_viagem-hr_descarga_fim INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ENDIF.
  ELSEIF  ls_good-fieldname = 'HR_DESCARGA_FIM' .
    CONVERT DATE gw_saida_viagem-dt_descarga_fim  TIME lv_value INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
  ENDIF.
  CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP vl_dt_fim TIME ZONE sy-zonlo.

  IF vl_dt_inicio > vl_dt_fim.
    vl_status = 4.
    RETURN.
  ENDIF.


*/// data da descarga fim tem q ser maior q a carga da descarga inicio
  CLEAR: vl_dt_inicio,
            vl_dt_fim.
  IF ls_good-fieldname = 'DT_DESCARGA_FIM'.
    IF gw_saida_viagem-hr_descarga_fim IS INITIAL.
      CONVERT DATE lv_value TIME '235959' INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE lv_value TIME gw_saida_viagem-hr_descarga_fim INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ENDIF.
  ELSEIF  ls_good-fieldname = 'HR_DESCARGA_FIM' .
    CONVERT DATE gw_saida_viagem-dt_descarga_fim  TIME lv_value INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
  ENDIF.

  CONVERT DATE gw_saida_viagem-dt_descarga_ini TIME gw_saida_viagem-hr_descarga_ini INTO TIME STAMP vl_dt_fim TIME ZONE sy-zonlo.

  IF vl_dt_inicio < vl_dt_fim.
    vl_status = 4.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_DT_DESCARGA
*&---------------------------------------------------------------------*
FORM f_valida_dt_descarga  USING    lv_value          TYPE lvc_value
                                   ls_good-fieldname TYPE lvc_s_modi-fieldname
                                   gw_saida_viagem   TYPE ty_saida_viagem
                                   vl_status         TYPE sy-subrc.

  CHECK lv_value IS NOT INITIAL.

  DATA: vl_dt_inicio TYPE timestamp,
        vl_dt_fim    TYPE timestamp,
        vl_date      TYPE d,
        vl_hour      TYPE sy-uzeit.

  CLEAR: vl_dt_inicio,
         vl_dt_fim.

  IF ls_good-fieldname = 'DT_DESCARGA_INI' .
    IF gw_saida_viagem-hr_descarga_ini IS INITIAL.
      CONVERT DATE lv_value TIME '235959' INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE lv_value TIME gw_saida_viagem-hr_descarga_ini INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ENDIF.
  ELSE.
    CONVERT DATE gw_saida_viagem-dt_descarga_ini TIME gw_saida_viagem-hr_descarga_ini INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
  ENDIF.

  IF ls_good-fieldname = 'DATA_CHEGADA'.
    CONVERT DATE lv_value TIME gw_saida_viagem-hr_prevista INTO TIME STAMP vl_dt_fim TIME ZONE sy-zonlo.
  ELSE.
    CONVERT DATE gw_saida_viagem-data_chegada TIME gw_saida_viagem-hr_prevista INTO TIME STAMP vl_dt_fim TIME ZONE sy-zonlo.
  ENDIF.

  IF vl_dt_inicio < vl_dt_fim.
    vl_status = 4.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_DT_INICIAL
*&---------------------------------------------------------------------*
FORM f_valida_dt_inicial  USING    lv_value          TYPE lvc_value
                                   ls_good-fieldname TYPE lvc_s_modi-fieldname
                                   gw_saida_viagem   TYPE ty_saida_viagem
                                   vl_status         TYPE sy-subrc.

  CHECK lv_value IS NOT INITIAL.

  DATA: vl_dt_inicio TYPE timestamp,
        vl_dt_fim    TYPE timestamp,
        vl_date      TYPE d,
        vl_hour      TYPE sy-uzeit.

  CLEAR: vl_dt_inicio,
              vl_dt_fim.

  IF ls_good-fieldname = 'DT_DESCARGA_INI'.
    IF gw_saida_viagem-hr_descarga_ini IS INITIAL.
      CONVERT DATE lv_value TIME '235959' INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE lv_value TIME gw_saida_viagem-hr_descarga_ini INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ENDIF.
  ELSEIF  ls_good-fieldname = 'HR_DESCARGA_INI'.
    CONVERT DATE gw_saida_viagem-dt_descarga_ini TIME lv_value INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
  ENDIF.

  CONVERT DATE gw_saida_viagem-data_chegada TIME gw_saida_viagem-hr_prevista INTO TIME STAMP vl_dt_fim TIME ZONE sy-zonlo.

  IF vl_dt_inicio < vl_dt_fim.
    vl_status = 4.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_DT_INI_CHEGADA
*&---------------------------------------------------------------------*
FORM f_valida_dt_ini_chegada  USING    lv_value          TYPE lvc_value
                                       ls_good-fieldname TYPE lvc_s_modi-fieldname
                                       gw_saida_viagem   TYPE ty_saida_viagem
                                       vl_status         TYPE sy-subrc.

  CHECK lv_value IS NOT INITIAL.

  DATA: vl_dt_inicio TYPE timestamp,
        vl_dt_fim    TYPE timestamp,
        vl_date      TYPE d,
        vl_hour      TYPE sy-uzeit.

  CLEAR: vl_dt_inicio,
         vl_dt_fim.

  IF ls_good-fieldname = 'DATA_CHEGADA'.
    IF gw_saida_viagem-hr_prevista IS INITIAL.
      CONVERT DATE lv_value TIME '235959' INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE lv_value TIME gw_saida_viagem-hr_prevista INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
    ENDIF.
  ELSEIF  ls_good-fieldname = 'HR_PREVISTA' .
    CONVERT DATE gw_saida_viagem-data_chegada TIME lv_value INTO TIME STAMP vl_dt_inicio TIME ZONE sy-zonlo.
  ENDIF.

*** BUG - 72072 - Inicio CSB
  IF ls_good-fieldname = 'DT_DESCARGA_INI'.
    IF gw_saida_viagem-dt_descarga_fim IS NOT INITIAL.
      IF lv_value > gw_saida_viagem-dt_descarga_fim.
        vl_status = 4.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ls_good-fieldname = 'DT_DESCARGA_FIM'.
    IF gw_saida_viagem-dt_descarga_ini IS INITIAL.
      vl_status = 4.
    ELSE.
      IF lv_value < gw_saida_viagem-dt_descarga_ini.
        vl_status = 4.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

*** BUG - 72072 - Fim CSB

  IF gw_saida_viagem-dt_descarga_ini IS NOT INITIAL. "CSB - 05.01.2021 - BUG - 70627
    CONVERT DATE gw_saida_viagem-dt_descarga_ini TIME gw_saida_viagem-hr_descarga_ini INTO TIME STAMP vl_dt_fim TIME ZONE sy-zonlo.
    IF vl_dt_inicio > vl_dt_fim.
      vl_status = 4.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ERRO
*&---------------------------------------------------------------------*
FORM f_display_erro  USING  limpa_celula
                            er_data_changed TYPE REF TO cl_alv_changed_data_protocol
                            ls_good-fieldname
                            ls_good-row_id
                            vl_msgv1
                            vl_msgv2
                            vl_msgv3
                            vl_msgv4.

  CALL METHOD er_data_changed->add_protocol_entry
    EXPORTING
      i_msgid     = 'FI'
      i_msgno     = '899'
      i_msgty     = 'E'
      i_msgv1     = vl_msgv1
      i_msgv2     = vl_msgv2
      i_msgv3     = vl_msgv3
      i_msgv4     = vl_msgv4
      i_fieldname = ls_good-fieldname
      i_row_id    = ls_good-row_id.

  IF limpa_celula = 'S'.
    CALL METHOD er_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = ls_good-fieldname
        i_value     = ''.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONSISTE_CCT
*&---------------------------------------------------------------------*
FORM f_consiste_cct  TABLES   lt_zlest0060_ck STRUCTURE zlest0060
                     USING   nfnum.
  DATA: ls_zlest0142 TYPE zlest0142.
  LOOP AT lt_zlest0060_ck ASSIGNING FIELD-SYMBOL(<fs_zlest0060_ck>).
    IF <fs_zlest0060_ck>-chave_nfe(1) EQ 'F'.
      SELECT SINGLE *
          FROM zlest0142 INTO ls_zlest0142
          WHERE chave_nff EQ <fs_zlest0060_ck>-chave_nfe.
    ELSE.
      SELECT SINGLE *
        FROM zlest0142 INTO ls_zlest0142
        WHERE chave_nfe EQ <fs_zlest0060_ck>-chave_nfe.
    ENDIF.
    IF sy-subrc = 0.
      nfnum = <fs_zlest0060_ck>-nfnum.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CCT
*&---------------------------------------------------------------------*
FORM f_check_cct  USING    ls_good-row_id
                  CHANGING nfnum.

  DATA: lt_zlest0060_ck TYPE TABLE OF zlest0060.

  READ TABLE gt_saida_viagem ASSIGNING FIELD-SYMBOL(<fs_saida_viagem>) INDEX ls_good-row_id.

  IF <fs_saida_viagem>-data_chegada IS INITIAL
    OR <fs_saida_viagem>-hr_prevista IS INITIAL
    OR <fs_saida_viagem>-peso_descarga IS INITIAL.
    CLEAR: nfnum.
    RETURN.
  ENDIF.

  CLEAR: lt_zlest0060_ck[].
  SELECT *
    FROM zlest0060 INTO TABLE lt_zlest0060_ck
    WHERE bukrs        EQ <fs_saida_viagem>-bukrs
      AND werks        EQ <fs_saida_viagem>-werks
      AND ano_viagem   EQ <fs_saida_viagem>-ano_viagem
      AND nr_viagem    EQ <fs_saida_viagem>-nr_viagem
      AND nome_emb     EQ <fs_saida_viagem>-nome_emb.

***///
  CLEAR: nfnum.
  PERFORM f_consiste_cct TABLES lt_zlest0060_ck
                         USING nfnum .
***///

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
FORM f_limpa_campos  USING   mens
                             limpa
                             ls_good-row_id.

  DATA: lt_zlest0061 TYPE TABLE OF zlest0061,
        ls_zlest0061 TYPE zlest0061,
        lt_zlest0060 TYPE TABLE OF zlest0060,
        ls_zlest0060 TYPE zlest0060,
        lv_vinc_cct  TYPE c.

  "BUG IMPEDITIVO 76565 Antes de limpar os campos verificar se já está vinculado ao cct LPORTELA

  READ TABLE gt_saida_viagem ASSIGNING FIELD-SYMBOL(<fs_saida_viagem>) INDEX ls_good-row_id.

  IF <fs_saida_viagem> IS ASSIGNED AND sy-subrc = 0.

    SELECT * FROM zlest0061
           INTO TABLE lt_zlest0061
         WHERE bukrs        EQ <fs_saida_viagem>-bukrs
           AND werks        EQ <fs_saida_viagem>-werks
           AND ano_viagem   EQ <fs_saida_viagem>-ano_viagem
           AND nr_viagem    EQ <fs_saida_viagem>-nr_viagem
           AND nome_emb     EQ <fs_saida_viagem>-nome_emb
           AND cod_material IN r_matnr.

    CHECK ( lt_zlest0061[] IS NOT INITIAL ) .

    SELECT *
      FROM zlest0060 INTO TABLE lt_zlest0060
       FOR ALL ENTRIES IN lt_zlest0061
     WHERE bukrs         EQ lt_zlest0061-bukrs
       AND werks         EQ lt_zlest0061-werks
       AND nr_viagem     EQ lt_zlest0061-nr_viagem
       AND ano_viagem    EQ lt_zlest0061-ano_viagem
       AND embarcacao    EQ lt_zlest0061-embarcacao
       AND nome_emb      EQ lt_zlest0061-nome_emb
       AND cl_codigo     EQ lt_zlest0061-cl_codigo
       AND nr_dco        EQ lt_zlest0061-nr_dco
       AND safra         EQ lt_zlest0061-safra
       AND docnum        EQ lt_zlest0061-docnum
       AND id_frete_aqua EQ lt_zlest0061-id_frete_aqua.

    LOOP AT lt_zlest0060 INTO ls_zlest0060.
      SELECT SINGLE  chave_nfe
         FROM zlest0142
        WHERE chave_nfe EQ @ls_zlest0060-chave_nfe
        INTO @DATA(lv_chave).

      IF lv_chave IS NOT INITIAL AND sy-subrc EQ 0.
        lv_vinc_cct = 'X'.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDIF.


  IF mens  = 'S' AND lv_vinc_cct IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Limpa Dados'
        text_question         = 'Os dados de Chegada e Descargas serão excluídos. Confirmar a Alteração?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
        popup_type            = 'ICON_MESSAGE_WARNING'
      IMPORTING
        answer                = answer.

  ENDIF.

  CLEAR: gva_not_change.

  IF ( mens = 'S' AND answer = 1 ) AND limpa = 'S'.

    " READ TABLE gt_saida_viagem ASSIGNING FIELD-SYMBOL(<fs_saida_viagem>) INDEX ls_good-row_id.

    IF <fs_saida_viagem> IS ASSIGNED AND sy-subrc = 0.

      CLEAR:  <fs_saida_viagem>-dt_descarga_ini,
              <fs_saida_viagem>-hr_descarga_ini,
              <fs_saida_viagem>-dt_descarga_fim,
              <fs_saida_viagem>-hr_descarga_fim,
              <fs_saida_viagem>-data_chegada,
              <fs_saida_viagem>-hr_prevista.


      CLEAR: lt_zlest0061[], lt_zlest0060[].
*     "// Verfica se o Material esta no SET caso esteja não considerar o material no select
      PERFORM fm_checar_material USING gw_saida_viagem-cod_material.

**<< "BUG IMPEDITIVO 76565 Antes de limpar os campos verificar se já está vinculado ao cct
*      SELECT * FROM zlest0061
*        INTO TABLE lt_zlest0061
*      WHERE bukrs        EQ <fs_saida_viagem>-bukrs
*        AND werks        EQ <fs_saida_viagem>-werks
*        AND ano_viagem   EQ <fs_saida_viagem>-ano_viagem
*        AND nr_viagem    EQ <fs_saida_viagem>-nr_viagem
*        AND nome_emb     EQ <fs_saida_viagem>-nome_emb
*        AND cod_material IN r_matnr.
*
*      CHECK ( lt_zlest0061[] IS NOT INITIAL ) .
*
*      SELECT *
*        FROM zlest0060 INTO TABLE lt_zlest0060
*         FOR ALL ENTRIES IN lt_zlest0061
*       WHERE bukrs         EQ lt_zlest0061-bukrs
*         AND werks         EQ lt_zlest0061-werks
*         AND nr_viagem     EQ lt_zlest0061-nr_viagem
*         AND ano_viagem    EQ lt_zlest0061-ano_viagem
*         AND embarcacao    EQ lt_zlest0061-embarcacao
*         AND nome_emb      EQ lt_zlest0061-nome_emb
*         AND cl_codigo     EQ lt_zlest0061-cl_codigo
*         AND nr_dco        EQ lt_zlest0061-nr_dco
*         AND safra         EQ lt_zlest0061-safra
*         AND docnum        EQ lt_zlest0061-docnum
*         AND id_frete_aqua EQ lt_zlest0061-id_frete_aqua.

*>> "BUG IMPEDITIVO 76565 Antes de limpar os campos verificar se já está vinculado ao cct

      CHECK ( lt_zlest0061[] IS NOT INITIAL ) .
      LOOP AT lt_zlest0061 INTO ls_zlest0061.
        UPDATE zlest0061
           SET dt_descarga_ini  = <fs_saida_viagem>-dt_descarga_ini
               hr_descarga_ini  = <fs_saida_viagem>-hr_descarga_ini
               dt_descarga_fim  = <fs_saida_viagem>-dt_descarga_fim
               hr_descarga_fim  = <fs_saida_viagem>-hr_descarga_fim
         WHERE docnum EQ ls_zlest0061-docnum.

        UPDATE zlest0060
           SET dt_descarga_ini  = <fs_saida_viagem>-dt_descarga_ini
               hr_descarga_ini  = <fs_saida_viagem>-hr_descarga_ini
               dt_descarga_fim  = <fs_saida_viagem>-dt_descarga_fim
               hr_descarga_fim  = <fs_saida_viagem>-hr_descarga_fim
         WHERE docnum EQ ls_zlest0061-docnum.
      ENDLOOP.

    ENDIF.
*** BUG - 72072 - Inicio - CBRAND
  ELSE.
    gva_not_change = 'N'.
    " "BUG IMPEDITIVO 76565
    MESSAGE e899(fi) DISPLAY LIKE 'W'  WITH 'NFe Vinculado a Esta barcaça já vinculado ao CCT'.


*** BUG - 72072 - Inicio - CBRAND
  ENDIF.

ENDFORM.

*** Inicio - Rubenilson Pereira - 16.06.2022 - 54384
*&---------------------------------------------------------------------*
*&      Form  F_REC_CARGA_CCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_rec_carga_cct USING p_ucomm TYPE sy-ucomm.

*** Disponibilizar CCT
  PERFORM disp_cct USING p_ucomm.

  PERFORM f_recepcao_carga.

  LEAVE TO SCREEN 0100.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gerar_token USING p_bukrs_ra CHANGING p_gravou TYPE c.  "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>

  "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
  CREATE OBJECT zcl_token_siscomex.

  zcl_token_siscomex->zif_cadastro~novo_registro( ).
  zcl_token_siscomex->set_bukrs( conv #( p_bukrs_ra ) ). "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
  zcl_token_siscomex->set_role_type( 'DEPOSIT' ). "Depositário

  zcl_token_siscomex->zif_cadastro~gravar_registro( RECEIVING i_gravou = p_gravou ).

*  CALL METHOD obj_grid_viagem->get_selected_rows
*    IMPORTING
*      et_index_rows = tl_rows.
*
*  IF tl_rows IS NOT INITIAL.
*    READ TABLE gt_saida_nfe ASSIGNING FIELD-SYMBOL(<fs_saida_nfe>) INDEX 1.
*    IF sy-subrc IS INITIAL.
*      SELECT bukrs_rom
*        FROM zlest0060
*        INTO @DATA(lv_bukrs_rom)
*        UP TO 1 ROWS
*        WHERE chave_nfe = @<fs_saida_nfe>-chave_nfe.
*      ENDSELECT.
*
*    ENDIF.
*
*    CREATE OBJECT zcl_token_siscomex.
*
*    zcl_token_siscomex->zif_cadastro~novo_registro( ).
*    zcl_token_siscomex->set_bukrs( conv #( lv_bukrs_rom ) ). "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
*    zcl_token_siscomex->set_role_type( 'DEPOSIT' ). "Depositário
*
*    zcl_token_siscomex->zif_cadastro~gravar_registro( RECEIVING i_gravou = p_gravou ).
*
*  ENDIF.
  "LES- US 153329- ZLES0088- Ajustes EUDR- WPP <----
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RECEPCAO_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_recepcao_carga .

  DATA: zcl_cct_recepcao_carga TYPE REF TO zcl_cct_recepcao_carga,
        lt_saida_nfe           TYPE TABLE OF ty_saida_nfe.

  DATA: lv_gravou TYPE c.

  "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
  DATA: lva_bukrs_ra  TYPE zlest0149-bukrs_ra,
        lva_branch_ra TYPE zlest0149-branch_ra.

  PERFORM f_get_branch_ra CHANGING lva_bukrs_ra lva_branch_ra.

  CHECK lva_branch_ra IS NOT INITIAL.

  SELECT SINGLE *
    FROM zlest0149 INTO @DATA(lwa_zlest0149)
   WHERE bukrs_ra  = @lva_bukrs_ra
     AND branch_ra = @lva_branch_ra.

  CHECK sy-subrc EQ 0.
  "LES- US 153329- ZLES0088- Ajustes EUDR- WPP <<---

  CALL METHOD obj_grid_viagem->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  CHECK tl_rows IS NOT INITIAL.

  LOOP AT tl_rows INTO sl_rows.

    READ TABLE gt_saida_nfe ASSIGNING FIELD-SYMBOL(<fs_saida_nfe>) INDEX sl_rows-index.
    IF sy-subrc IS INITIAL AND <fs_saida_nfe>-disp_cct EQ icon_okay.
      APPEND <fs_saida_nfe> TO lt_saida_nfe.
    ENDIF.
  ENDLOOP.




  IF lt_saida_nfe IS NOT INITIAL.

    PERFORM f_gerar_token USING lva_bukrs_ra CHANGING lv_gravou.

    CHECK lv_gravou IS NOT INITIAL.

    DATA(lt_saida_nfe_aux) = lt_saida_nfe.

    SORT lt_saida_nfe_aux BY chave_nfe.
    DELETE ADJACENT DUPLICATES FROM lt_saida_nfe_aux COMPARING chave_nfe.

    SELECT chave_nfe,
           bukrs_rom,
           branch_rom
      FROM zlest0060
      INTO TABLE @DATA(lt_zles0060)
      FOR ALL ENTRIES IN @lt_saida_nfe_aux
      WHERE chave_nfe = @lt_saida_nfe_aux-chave_nfe.
    IF sy-subrc IS INITIAL.
      SORT lt_zles0060 BY chave_nfe.

      DATA(lt_zles0060_aux) = lt_zles0060.

      SORT lt_zles0060_aux BY bukrs_rom branch_rom.
      DELETE ADJACENT DUPLICATES FROM lt_zles0060_aux COMPARING bukrs_rom branch_rom.

      "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
*         SELECT *
*        FROM zlest0149
*        INTO TABLE @DATA(lt_zlest0149)
*        FOR ALL ENTRIES IN @lt_zles0060_aux
*        WHERE bukrs_ra  = @lt_zles0060_aux-bukrs_rom
*          AND branch_ra = @lt_zles0060_aux-branch_rom.
*      IF sy-subrc IS INITIAL.
*        SORT lt_zlest0149 BY bukrs_ra branch_ra.
*      ENDIF.

      "LES- US 153329- ZLES0088- Ajustes EUDR- WPP <<---
    ENDIF.

    SELECT *
      FROM zlest0142
      INTO TABLE @DATA(lt_zlest0142)
      FOR ALL ENTRIES IN @lt_saida_nfe_aux
      WHERE chave_nff = @lt_saida_nfe_aux-chave_nfe
         OR chave_nfe = @lt_saida_nfe_aux-chave_nfe.
    IF sy-subrc IS INITIAL.
      DATA(lt_zlest0142f) = lt_zlest0142.
      SORT lt_zlest0142f BY chave_nff.
      SORT lt_zlest0142 BY chave_nfe.
    ENDIF.

    LOOP AT tl_rows INTO sl_rows.

      FREE zcl_cct_recepcao_carga.
      CREATE OBJECT zcl_cct_recepcao_carga.
      zcl_cct_recepcao_carga->zif_cadastro~novo_registro( ).

      READ TABLE gt_saida_nfe ASSIGNING <fs_saida_nfe> INDEX sl_rows-index.
      IF sy-subrc IS INITIAL AND <fs_saida_nfe>-disp_cct EQ icon_okay.

        READ TABLE lt_zles0060 ASSIGNING FIELD-SYMBOL(<fs_zles0060>)
        WITH KEY chave_nfe = <fs_saida_nfe>-chave_nfe
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          IF <fs_zles0060>-chave_nfe(1) EQ 'F' .

            zcl_cct_recepcao_carga->set_tp_recepcao( '2' ).

            READ TABLE lt_zlest0142 ASSIGNING FIELD-SYMBOL(<fs_zlest0142>)
            WITH KEY chave_nff = <fs_zles0060>-chave_nfe
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              zcl_cct_recepcao_carga->set_transportador_cnpj( <fs_zlest0142>-cnpj_transp ).
              zcl_cct_recepcao_carga->set_peso_aferido_recepcao( <fs_zlest0142>-peso_chegada ).

            ENDIF.

            zcl_cct_recepcao_carga->add_nff(
              EXPORTING
                i_chave_nff   = <fs_zles0060>-chave_nfe
                i_complemento = space ).

          ELSE.

            zcl_cct_recepcao_carga->set_tp_recepcao( '1' ).

            READ TABLE lt_zlest0142 ASSIGNING <fs_zlest0142>
            WITH KEY chave_nfe = <fs_zles0060>-chave_nfe
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              zcl_cct_recepcao_carga->set_transportador_cnpj( <fs_zlest0142>-cnpj_transp ).
              zcl_cct_recepcao_carga->set_peso_aferido_recepcao( <fs_zlest0142>-peso_chegada ).

            ENDIF.

            zcl_cct_recepcao_carga->add_nfe(
              EXPORTING
                i_chave_nfe   = <fs_zles0060>-chave_nfe
                i_complemento = space ).

          ENDIF.

          "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
*          READ TABLE lt_zlest0149 ASSIGNING FIELD-SYMBOL(<fs_zlest0149>)
*          WITH KEY bukrs_ra  = <fs_zles0060>-bukrs_rom
*                   branch_ra = <fs_zles0060>-branch_rom
*          BINARY SEARCH.
          "LES- US 153329- ZLES0088- Ajustes EUDR- WPP <<---

          "IF lwa_zlest0149 IS not INITIAL. "LES- US 153329- ZLES0088- Ajustes EUDR- WPP <<---

          zcl_cct_recepcao_carga->set_bukrs( lwa_zlest0149-bukrs_ra ). "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
          zcl_cct_recepcao_carga->set_branch( lwa_zlest0149-branch_ra ). "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
          zcl_cct_recepcao_carga->set_cnpj_responsavel( ).
          zcl_cct_recepcao_carga->set_local_codigo_urf( lwa_zlest0149-local_codigo_urf ). "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>
          zcl_cct_recepcao_carga->set_local_codigo_ra( lwa_zlest0149-local_codigo_ra ). "LES- US 153329- ZLES0088- Ajustes EUDR- WPP --->>

          zcl_cct_recepcao_carga->set_token( zcl_token_siscomex ). "Set token para Validação.
          zcl_cct_recepcao_carga->zif_cadastro~gravar_registro( RECEIVING i_gravou = lv_gravou ).

          IF lv_gravou IS NOT INITIAL.
            <fs_saida_nfe>-recep_cct = icon_okay.
          ENDIF.

          "ENDIF.  "LES- US 153329- ZLES0088- Ajustes EUDR- WPP <<---

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.

*** Fim - Rubenilson Pereira - 16.06.2022 - 54384


FORM f_get_branch_ra CHANGING c_bukrs_ra
                              c_branch_ra.

  DATA: lv_kunnr  TYPE kunnr,
        lv_xwerks TYPE werks_d.

  CLEAR: c_bukrs_ra, c_branch_ra.

  SELECT SINGLE *
    FROM zlest0056 INTO @DATA(lwa_zlest0056)
   WHERE bukrs      EQ @wp_tela_0100-wp_bukrs
     AND werks      EQ @wp_tela_0100-wp_werks
     AND ano_viagem EQ @wp_tela_0100-wp_ano
     AND nr_viagem  EQ @wp_tela_0100-wp_viagem.

  CHECK sy-subrc EQ 0.

  TRY.

      lv_kunnr = |{ lwa_zlest0056-po_destino ALPHA = IN  }|.

      zcl_clientes=>zif_parceiros~get_instance(
      )->set_parceiro( i_parceiro = lv_kunnr
      )->ck_parceiro_local_negocio(  ).

      lv_xwerks = |{ lwa_zlest0056-po_destino ALPHA = IN  }|.

      SELECT SINGLE *
        FROM j_1bbranch INTO @DATA(lwa_branch)
        WHERE branch EQ @lv_xwerks.

      CHECK sy-subrc EQ 0.

      SELECT SINGLE *
        FROM zlest0149 INTO @DATA(lwa_zlest0149)
       WHERE bukrs_ra  EQ @lwa_branch-bukrs
         AND branch_ra EQ @lwa_branch-branch.

      CHECK sy-subrc EQ 0.

      c_bukrs_ra  = lwa_zlest0149-bukrs_ra.
      c_branch_ra = lwa_zlest0149-branch_ra.

    CATCH zcx_parceiros.
  ENDTRY.


ENDFORM.
