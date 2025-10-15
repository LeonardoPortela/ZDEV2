REPORT  zlesr0081.

*/===========================================================================\*
*| TABLES
*\===========================================================================/*
TABLES: zciot_webservice.

*/===========================================================================\*
*| CONSTANTS
*\===========================================================================/*
CONSTANTS: gt_tela_0100 TYPE sy-dynnr VALUE '0100',
           gt_tela_0300 TYPE sy-dynnr VALUE '0300'.

*/===========================================================================\*
*| TYPES
*\===========================================================================/*
TYPES: BEGIN OF ty_tela_edit,
         tipo         TYPE zciot_webservice-tipo,
         servico      TYPE zciot_webservice-servico,
         url          TYPE zciot_webservice-url,
         content_type TYPE zciot_webservice-content_type,
         usuario      TYPE zciot_webservice-usuario,
         senha        TYPE zciot_webservice-senha,
         url_token    TYPE zciot_webservice-url_token,
       END OF ty_tela_edit,

       BEGIN OF ty_saida,
         opcao        TYPE c LENGTH 4,
         servico      TYPE c LENGTH 60,
         tipo         TYPE c LENGTH 60,
         url          TYPE zciot_webservice-url,
         content_type TYPE zciot_webservice-content_type,
         usuario      TYPE zciot_webservice-usuario,
         senha        TYPE zciot_webservice-senha,
         url_token    TYPE zciot_webservice-url_token,
       END OF ty_saida.

*/===========================================================================\*
*| CLASSES
*\===========================================================================/*
DATA: cl_container TYPE REF TO cl_gui_custom_container,
      cl_grid      TYPE REF TO cl_gui_alv_grid.

*/===========================================================================\*
*| INTERNAL TABLE
*\===========================================================================/*
DATA: gt_fcatalog         TYPE lvc_t_fcat,
      gt_saida            TYPE TABLE OF ty_saida,
      gt_zciot_webservice TYPE TABLE OF zciot_webservice,
      gt_ztipowebadm      TYPE TABLE OF dd07v,
      gt_ztipowebserv     TYPE TABLE OF dd07v.

*/===========================================================================\*
*| WORK AREA
*\===========================================================================/*
DATA: gw_tela_edit        TYPE ty_tela_edit,
      gw_fcatalog         TYPE lvc_s_fcat,
      gw_saida            TYPE ty_saida,
      gw_zciot_webservice TYPE zciot_webservice,
      gw_stable           TYPE lvc_s_stbl.


*/===========================================================================\*
*| SELEÇÃO
*\===========================================================================/*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_serv    FOR zciot_webservice-servico NO-EXTENSION NO INTERVALS,
                  p_tipo    FOR zciot_webservice-tipo NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN: END OF BLOCK b1.

*/===========================================================================\*
*| CLASSES
*\===========================================================================/*
CLASS cl_eventos DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "cl_eventos DEFINITION

*----------------------------------------------------------------------*
*       CLASS CL_EVENTOS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_eventos IMPLEMENTATION.

  METHOD handle_hotspot_click.

    CASE e_column_id.
      WHEN: 'OPCAO'.

        DELETE gt_saida INDEX e_row_id.

*** MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
        CALL METHOD cl_grid->refresh_table_display
          EXPORTING
            is_stable = gw_stable.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

ENDCLASS.                    "cl_eventos IMPLEMENTATION


*/===========================================================================\*
*| CALLS
*\===========================================================================/*
START-OF-SELECTION.
  PERFORM: selecionar_dados,
           criar_alv.

  CALL SCREEN gt_tela_0100.

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       PBO
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*     PAI
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  CASE sy-ucomm.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
    WHEN: 'NOVO'.
      CALL SCREEN 0300 STARTING AT 10 10.
  ENDCASE.

*** MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
  CALL METHOD cl_grid->refresh_table_display
    EXPORTING
      is_stable = gw_stable.

ENDMODULE.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM criar_alv .

  IF ( cl_container IS INITIAL ).

    CREATE OBJECT cl_container
      EXPORTING
        container_name              = 'CONTAINER_PRINCIPAL'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT cl_grid
      EXPORTING
        i_parent          = cl_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM: criar_catalog.


    SET HANDLER: cl_eventos=>handle_hotspot_click FOR cl_grid.

    CALL METHOD cl_grid->set_table_for_first_display
      CHANGING
        it_outtab                     = gt_saida[]
        it_fieldcatalog               = gt_fcatalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
*** MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
    CALL METHOD cl_grid->refresh_table_display
      EXPORTING
        is_stable = gw_stable.

  ENDIF.


ENDFORM.                    " CRIAR_ALV
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG
*&---------------------------------------------------------------------*
FORM criar_catalog .

  REFRESH: gt_fcatalog.

  PERFORM estrutura_alv USING:

        0 'ZCIOT_WEBSERVICE'  ' ' 'IT_SAIDA' 'OPCAO'        'Opção'               '5'   'X' '' ''  'C',
        1 'ZCIOT_WEBSERVICE'  ' ' 'IT_SAIDA' 'SERVICO'      'Tipo de Serviço'     '20'  '' '' ''  '' ,
        1 'ZCIOT_WEBSERVICE'  ' ' 'IT_SAIDA' 'TIPO'         'Modo'                '20'  '' '' ''  '' ,
        1 'ZCIOT_WEBSERVICE'  ' ' 'IT_SAIDA' 'URL'          'Endereço'            '80'  '' '' ''  '' ,
        1 'ZCIOT_WEBSERVICE'  ' ' 'IT_SAIDA' 'CONTENT_TYPE' 'Content Type'        '70'  '' '' ''  '' ,
        1 'ZCIOT_WEBSERVICE'  ' ' 'IT_SAIDA' 'USUARIO'      'Usuário'             '20'  '' '' ''  '' ,
        1 'ZCIOT_WEBSERVICE'  ' ' 'IT_SAIDA' 'SENHA'        'Senha'               '20'  '' '' ''  '' ,
        1 'ZCIOT_WEBSERVICE'  ' ' 'IT_SAIDA' 'URL_TOKEN'    'Endereço Token OPUS' '80'  '' '' ''  '' .

ENDFORM.                    " CRIAR_CATALOG

*&---------------------------------------------------------------------*
*&      Form  ESTRUTURA_ALV
*&---------------------------------------------------------------------*
FORM estrutura_alv  USING    VALUE(p_col_pos)       TYPE i
                             VALUE(p_ref_tabname)   LIKE dd02d-tabname
                             VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                             VALUE(p_tabname)       LIKE dd02d-tabname
                             VALUE(p_field)         LIKE dd03d-fieldname
                             VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                             VALUE(p_outputlen)
                             VALUE(p_hotspot)
                             VALUE(p_sum)
                             VALUE(p_emphasize)
                             VALUE(p_just).



  CLEAR gw_fcatalog.

  gw_fcatalog-fieldname   = p_field.
  gw_fcatalog-tabname     = p_tabname.
  gw_fcatalog-ref_table   = p_ref_tabname.
  gw_fcatalog-ref_field   = p_ref_fieldname.
  gw_fcatalog-key         = ' '.
  gw_fcatalog-col_pos     = p_col_pos.
  gw_fcatalog-outputlen   = p_outputlen.
  gw_fcatalog-no_out      = ' '.
  gw_fcatalog-reptext     = p_scrtext_l.
  gw_fcatalog-scrtext_s   = p_scrtext_l.
  gw_fcatalog-scrtext_m   = p_scrtext_l.
  gw_fcatalog-scrtext_l   = p_scrtext_l.
  gw_fcatalog-emphasize   = p_emphasize.
  gw_fcatalog-just        = p_just.
  gw_fcatalog-hotspot     = p_hotspot.

  APPEND gw_fcatalog TO gt_fcatalog.

ENDFORM.                    " ESTRUTURA_ALV


*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM selecionar_dados .

  "Selecionar dados da tabela ZCIOT_WEBSERVICE
  SELECT * FROM zciot_webservice
    INTO TABLE gt_zciot_webservice
   WHERE servico IN p_serv
     AND tipo    IN p_tipo.


** Get Descrição de cada Item da URL
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZTIPOWEBADM'
      text            = abap_true
    TABLES
      values_tab      = gt_ztipowebadm
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

** Get Descrição de cada Item da URL
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZTIPOWEBSERV'
      text            = abap_true
    TABLES
      values_tab      = gt_ztipowebserv
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  CHECK NOT gt_zciot_webservice[] IS INITIAL.

  LOOP AT gt_zciot_webservice INTO gw_zciot_webservice.

    CLEAR: gw_saida.

    gw_saida-opcao   = icon_delete.

    TRY .
        gw_saida-servico = gt_ztipowebserv[  domvalue_l = gw_zciot_webservice-servico ]-ddtext.
      CATCH cx_sy_itab_line_not_found.
        CLEAR gw_saida-servico.
    ENDTRY.

    TRY .
        gw_saida-tipo = gt_ztipowebadm[  domvalue_l = gw_zciot_webservice-tipo ]-ddtext.
      CATCH cx_sy_itab_line_not_found.
        CLEAR gw_saida-tipo.
    ENDTRY.

* "// remove os - e os espaçoes de Inicio da String
    IF gw_saida-servico IS NOT INITIAL.
      DO.
        CASE gw_saida-servico(1).
          WHEN '-' OR ' '.
            gw_saida-servico = |{ gw_saida-servico+1 }|.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDDO.
    ENDIF.

    gw_saida-servico = |{ gw_zciot_webservice-servico } - { gw_saida-servico }|.

    CASE gw_zciot_webservice-servico.

      WHEN: '11'.
        gw_saida-servico = 'Permissão Usuario ( POST )'.
        gw_saida-tipo    = '9'.

      WHEN: '10'.
        gw_saida-servico = '10 - Cancelamento de ordem carregamento'.
        gw_saida-tipo    = 'O'.
      WHEN: 'UN'.
        gw_saida-servico = 'Integração SAP x Unidata ( GET )'.
        gw_saida-tipo    = 'UNIDATA'.

      WHEN: 'UD'.
        gw_saida-servico = 'Integração SAP x Unidata ( PUT )'.
        gw_saida-tipo    = 'UNIDATA'.
      WHEN: 'KU'.
        gw_saida-servico = 'Webservice HVI Kuhlmann'.
        gw_saida-tipo    = 'Kuhlmann'.
      WHEN: 'OT'.
        gw_saida-servico = 'OT - Webservice HVI Kuhlmann'.
        gw_saida-tipo    = 'Kuhlmann'.
      WHEN: 'YN'.
        gw_saida-servico = 'YN- Simetrya - NF-e Terceiro'.
        gw_saida-tipo    = 'Simetrya'.
      WHEN: 'YC'.
        gw_saida-servico = 'YN- Simetrya - CT-e Terceiro'.
        gw_saida-tipo    = 'Simetrya'.
      WHEN: 'IT'.
        gw_saida-servico = 'IT  - Integração OPUS - Romaneio Completo'.
        gw_saida-tipo    = 'OPUS'.
      WHEN: 'IC'.
        gw_saida-servico = 'IC  - Integração OPUS - Romaneio Completo - Cancelamento'.
        gw_saida-tipo    = 'OPUS'.
      WHEN: 'CR'.
        gw_saida-servico = 'CR  - Integração OPUS - Consulta Romaneio'.
        gw_saida-tipo    = 'OPUS'.
      WHEN: 'IA'.
        gw_saida-servico = 'IA  - Integração OPUS - Romaneio Completo - Alteração'.
        gw_saida-tipo    = 'OPUS'.
      WHEN: 'IR'.
        gw_saida-servico = 'IR  - Integração OPUS - Romaneio Completo - PDF Romaneio'.
        gw_saida-tipo    = 'OPUS'.
      WHEN: 'IK'.
        gw_saida-servico = 'IK  - Integração OPUS - Romaneio Completo - Ticket Romaneio'.
        gw_saida-tipo    = 'OPUS'.
      WHEN: 'IM'.
        gw_saida-servico = 'IM  - Integração OPUS - Lista de Impressas'.
        gw_saida-tipo    = 'OPUS'.
      WHEN: 'P1'.
        gw_saida-servico = 'P1 - Consulta Python SQL'.
        gw_saida-tipo    = 'Python'.
      WHEN: 'LS'.
        gw_saida-servico = 'LS - Integração SAP x ALS (s360) - Coletar TOKEN'.
        gw_saida-tipo    = 'L - Analise de óleo'.
      WHEN: 'SL'.
        gw_saida-servico = 'SL - Resultado amostra SAP x ALS (s360)'.
        gw_saida-tipo    = 'L - Analise de óleo'.
      WHEN: 'AN'.
        gw_saida-servico = 'AN - Listar todas as amostras SAP x ALS (s360)'.
        gw_saida-tipo    = 'L - Analise de óleo'.
      WHEN: 'IS'.
        gw_saida-servico = 'IS - Integração SAP x ALS (s360) - PRD'.
        gw_saida-tipo    = 'L - Analise de óleo'.
      WHEN: 'LW'.
        gw_saida-servico = 'LW - Integração SAP x ALS (s360) - Cadastro amostra'.
        gw_saida-tipo    = 'L - Analise de óleo'.
      WHEN: 'LT'.
        gw_saida-servico = 'LT - Integração SAP x ALS (s360) - Cadastro equipamento'.
        gw_saida-tipo    = 'L - Analise de óleo'.
      WHEN: 'LC'.
        gw_saida-servico = 'LC - Integração SAP x ALS (s360) - Cadastro compartimento'.
        gw_saida-tipo    = 'L - Analise de óleo'.
      WHEN: '20'.
        gw_saida-servico = '20 - SAP x Autotrac - Consulta dados veiculo'.
        gw_saida-tipo    = '7  - SAP x Autotrac'.
      WHEN: '21'.
        gw_saida-servico = '21 - SAP x Autotrac - Consulta dados veiculo'.
        gw_saida-tipo    = '7  - SAP x Autotrac'.
      WHEN: '22'.
        gw_saida-servico = '22 - SAP x Autotrac - Consulta dados veiculo próprio Amaggi'.
        gw_saida-tipo    = '7  - SAP x Autotrac'.

      WHEN: '23'.
        gw_saida-servico = 'Token - ACTS - Fardinho Tracecotton'.
        gw_saida-tipo    = '6'.
      WHEN '24'.
        gw_saida-servico = 'Get  ACTS - Fardinho Tracecotton'.
        gw_saida-tipo    = '6'.

      WHEN: 'RV'.
        gw_saida-servico = 'RV - Modal Rodoviario ( GET )'.
        gw_saida-tipo    = '8'.

      WHEN 'RF'.
        gw_saida-servico = 'RF - Modal Ferroviario ( GET )'.
        gw_saida-tipo    = '8'.

      WHEN 'T9'.
        gw_saida-servico = 'T9 - Token Opus ( POST )'.
        gw_saida-tipo    = '8'.

      WHEN 'ST'.
        gw_saida-servico = 'ST - Situalção transportador SAP/TIP  ( POST )'.
        gw_saida-tipo    = 'S'.
      WHEN 'SP'.
        gw_saida-servico = 'SP - Status parceiro SAP/TIP  ( POST )'.
        gw_saida-tipo    = 'S'.



      WHEN: 'SE'.
        CASE gw_zciot_webservice-tipo.
          WHEN 1.
            gw_saida-tipo = 'New WorkFlow'.
          WHEN 2.
            gw_saida-tipo = 'Cancel WorkFlow'.
          WHEN 3.
            gw_saida-tipo = 'Edit Attibutes WorkFlow'.
          WHEN 4.
            gw_saida-tipo = 'Edit Entitys WorkFlow'.
        ENDCASE.
        gw_saida-servico = 'Integração SoftExpert'.
      WHEN: 'VI'.
        gw_saida-servico = 'Viagem.'.
        CASE gw_zciot_webservice-tipo.
          WHEN: 'I'.
            gw_saida-tipo = 'Cadastro de Viagem'.
          WHEN: 'C'.
            gw_saida-tipo = 'Consulta de Viagem'.
          WHEN: 'F'.
            gw_saida-tipo = 'Confirmação de Viagem'.
          WHEN: 'E'.
            gw_saida-tipo = 'Cancelamento de Viagem'.
          WHEN: 'L'.
            gw_saida-tipo = 'Analise de óleo'.
        ENDCASE.
      WHEN: 'PE'.
        gw_saida-servico = 'Pedágio.'.
        CASE gw_zciot_webservice-tipo.
          WHEN: 'I'.
            gw_saida-tipo = 'Cadastro de Viagem'.
          WHEN: 'C'.
            gw_saida-tipo = 'Consulta de Viagem'.
          WHEN: 'F'.
            gw_saida-tipo = 'Confirmação de Viagem'.
          WHEN: 'E'.
            gw_saida-tipo = 'Cancelamento de Viagem'.
        ENDCASE.
      WHEN: 'R1'.
        gw_saida-servico = 'REPOM'.
        gw_saida-tipo    = 'WebServices REPOM - Rota/Pedágio'.
      WHEN: 'DC'.
        gw_saida-servico = 'OPUS'.
        gw_saida-tipo    = 'Resultado de Classificação'.
      WHEN: 'RT'.
        gw_saida-servico = 'OPUS'.
        gw_saida-tipo    = 'Restrição de Produtor'.
      WHEN: 'WH'.
        gw_saida-servico = 'HCM'.
        gw_saida-tipo    = 'WebServices WEBSEMPRE - Movimentações'.

      WHEN: '08'.
        CASE gw_zciot_webservice-tipo.
          WHEN 'RV'.
            gw_saida-servico = 'Modal Rodoviario ( GET )'.
            gw_saida-tipo    = 'RV'.
          WHEN 'RF'.
            gw_saida-servico = 'Modal Ferroviario ( GET )'.
            gw_saida-tipo    = 'RF'.
          WHEN 'T9'.
            gw_saida-servico = 'Token Opus ( POST )'.
            gw_saida-tipo    = 'T9'.
        ENDCASE.

    ENDCASE.

    CASE gw_zciot_webservice-tipo.
      WHEN 'O'.
        CASE gw_zciot_webservice-servico.
          WHEN '01'.
            gw_saida-servico = 'OPUS'.
            gw_saida-tipo    = 'OPUS - CARGUERO - Lote de Frete'.

          WHEN 'SA'.
            gw_saida-servico = 'OPUS'.
            gw_saida-tipo    = 'OPUS - Saldo de Algodão'.

          WHEN 'XM'.
            gw_saida-servico = 'OPUS'.
            gw_saida-tipo    = 'Envio de XML de Doc. Fiscal para o OPUS'.

          WHEN 'BA'.
            gw_saida-servico = 'OPUS'.
            gw_saida-tipo    = 'URL BASE OPUS'.

        ENDCASE.
    ENDCASE.

    gw_saida-url          = gw_zciot_webservice-url.
    gw_saida-content_type = gw_zciot_webservice-content_type.
    gw_saida-usuario      = gw_zciot_webservice-usuario.
    gw_saida-senha        = gw_zciot_webservice-senha.
    gw_saida-url_token    = gw_zciot_webservice-url_token.
    APPEND gw_saida TO gt_saida.

  ENDLOOP.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR 'TB0300'.
ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0300 INPUT.
  CASE sy-ucomm.
    WHEN: 'SALVAR'.
      PERFORM: salvar_endereco.
  ENDCASE.
ENDMODULE.                 " PAI_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  SALVAR_ENDERECO
*&---------------------------------------------------------------------*
FORM salvar_endereco .

  DATA: input_endereco  TYPE zciot_webservice.

  input_endereco-servico      = gw_tela_edit-servico.
  input_endereco-tipo         = gw_tela_edit-tipo.
  input_endereco-url          = gw_tela_edit-url.
  input_endereco-content_type = gw_tela_edit-content_type.
  input_endereco-usuario      = gw_tela_edit-usuario.
  input_endereco-senha        = gw_tela_edit-senha.
  input_endereco-url_token    = gw_tela_edit-url_token.
  MODIFY zciot_webservice FROM input_endereco.

  COMMIT WORK.

  CLEAR: input_endereco-servico,
         input_endereco-tipo,
         input_endereco-url,
         input_endereco-content_type,
         input_endereco-senha,
         input_endereco-usuario,
         input_endereco-url_token.

  gw_saida-servico      = gw_tela_edit-servico.
  gw_saida-tipo         = gw_tela_edit-tipo.
  gw_saida-url          = gw_tela_edit-url.
  gw_saida-content_type = gw_tela_edit-content_type.
  gw_saida-usuario      = gw_tela_edit-usuario.
  gw_saida-senha        = gw_tela_edit-senha.
  gw_saida-url_token    = gw_tela_edit-url_token.

  APPEND gw_saida TO gt_saida.

  CLEAR: gw_tela_edit-servico,
         gw_tela_edit-tipo,
         gw_tela_edit-url,
         gw_tela_edit-content_type,
         gw_tela_edit-usuario,
         gw_tela_edit-senha,
         gw_tela_edit-url_token.

  CLEAR: input_endereco, gw_saida.

  LEAVE TO SCREEN 0.

ENDFORM.                    " SALVAR_ENDERECO

*&---------------------------------------------------------------------*
*&      Module  PAI_0300_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0300_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
