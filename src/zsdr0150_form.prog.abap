*----------------------------------------------------------------------*
***INCLUDE ZSDR0150_FORM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_SETTING_VERIFY_FIELD
*&---------------------------------------------------------------------*
*       Configura  o e valida  o do respectivo campo acionado
*----------------------------------------------------------------------*
*    --> P_VALUE Valor do respectivo campo para validar o preenchimento
*    --> P_NAMEF Nome do respectivo campo para validar o preenchimento
*    --> P_DYNNR Valor da tela a qual o campo pertence
*----------------------------------------------------------------------*
FORM zf_setting_verify_field USING p_value
                                   p_namef
                                   p_dynnr TYPE sydynnr.

  CONSTANTS: cl_onli TYPE char4 VALUE 'ONLI'.

  IF sscrfields-ucomm EQ cl_onli  AND
     p_dynnr          EQ sy-dynnr.

    IF ( p_insumo IS NOT INITIAL       AND
         rb_venda IS NOT INITIAL       AND
       ( p_namef  EQ 'S_VKORG-LOW'     OR
         p_namef  EQ 'S_VKBUR-LOW' ) ) OR
       ( p_insumo IS NOT INITIAL       AND
         rb_compr IS NOT INITIAL       AND
         p_namef  EQ 'S_BUKRS-LOW' ).
      DATA(vl_check) = abap_on.

    ENDIF.

    IF     p_value  IS INITIAL AND
       NOT vl_check IS INITIAL.
* Preencher todos os campos obrigatórios
      MESSAGE e024(sd) WITH TEXT-300.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_verify_authority_check
*&---------------------------------------------------------------------*
*& Verifica se tem permissão para executar a opção da tela
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_verify_authority_check USING uv_direcao TYPE zsded_tp_direcao
                                     uv_area    TYPE zsded_area
                                     uv_bukrs   TYPE bukrs
                                     uv_vkbur   TYPE vkbur.

  AUTHORITY-CHECK OBJECT 'ZSDT0203'
   ID 'Z_DIRECAO' FIELD uv_direcao
   ID 'Z_AREA'    FIELD uv_area
   ID 'BUKRS'     FIELD uv_bukrs
   ID 'VKBUR'     FIELD uv_vkbur.
  IF NOT sy-subrc IS INITIAL.
* Sem autorização para opção &1/&2/&3/&4, procurar Area de Insumos para avaliarem seu acesso.
    DATA(vl_text) = CONV string( TEXT-e02 ).
    REPLACE ALL OCCURRENCES OF '&1' IN vl_text WITH uv_direcao.
    REPLACE ALL OCCURRENCES OF '&2' IN vl_text WITH uv_area.
    REPLACE ALL OCCURRENCES OF '&3' IN vl_text WITH uv_bukrs.
    REPLACE ALL OCCURRENCES OF '&4' IN vl_text WITH uv_vkbur.

    MESSAGE vl_text TYPE 'E'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_data_authority_check
*&---------------------------------------------------------------------*
*& ados para verificar permissão para executa a opção da tela
*&---------------------------------------------------------------------*
*& -->  PR_BUKRS  Select Opotion Empresa/Organização de Vendas
*& -->  PR_VKBUR  Select Opotion Escritório de Venda
*& -->  PR_WERKS  Select Opotion Local de Negócio
*& -->  UP_INSUMO Botão Radio Insumo (Área)
*& -->  UP_MERINT Botão Radio Mercado Interno (Área)
*& -->  URB_VENDA Botão Radio Venda (Tipo de Insumo)
*& -->  URB_COMPR Botão Radio Compra (Tipo de Insumo)
*&---------------------------------------------------------------------*
FORM zf_data_authority_check TABLES pr_bukrs  TYPE STANDARD TABLE
                                    pr_vkbur  TYPE STANDARD TABLE
                                    pr_werks  TYPE STANDARD TABLE
                              USING uv_insumo TYPE c
                                    uv_merint TYPE c
                                    uv_venda  TYPE c
                                    uv_compr  TYPE c.

  DATA: vl_direcao TYPE zsded_tp_direcao,
        vl_area    TYPE zsded_area,
        vl_bukrs   TYPE bukrs,
        vl_vkbur   TYPE vkbur.

  CASE abap_on.
    WHEN uv_insumo. "Insumos
      vl_area = 01.
      SELECT bukrs        FROM tvko  INTO TABLE @DATA(tl_bukrs) WHERE vkorg IN @pr_bukrs.
      SELECT vkorg, vkbur FROM tvkbz INTO TABLE @DATA(tl_tvkbz) WHERE vkorg IN @pr_bukrs
                                                                  AND vkbur IN @pr_vkbur.

      CASE abap_on.
        WHEN uv_venda. "Vendas
          vl_direcao = sy-abcde+21(1). "V - Vendas

        WHEN uv_compr. "Compras
          vl_direcao = sy-abcde+2(1).  "C - Compras

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

    WHEN uv_merint. "Mercado Interno
      vl_area    = 02.
      vl_direcao = sy-abcde+21(1). "V - Vendas
      vl_vkbur   = '*'.
      SELECT bukrs FROM t001 INTO TABLE tl_bukrs WHERE bukrs IN pr_bukrs.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

  LOOP AT tl_bukrs INTO vl_bukrs.
    IF NOT uv_insumo IS INITIAL.
      LOOP AT pr_vkbur.
        IF line_exists( tl_tvkbz[ vkorg = vl_bukrs vkbur = pr_vkbur-('LOW') ] ).
          DATA(vl_flag) = abap_on.

        ELSE.
* O Escr. Vds & não pertence a(s) Org. de Vnd(s) Informado na tela de filtro
          DATA(vl_text) = CONV string( TEXT-e01 ).
          REPLACE ALL OCCURRENCES OF '&' IN vl_text WITH pr_vkbur-('LOW').
          MESSAGE vl_text TYPE 'E'.

        ENDIF.

      ENDLOOP.

    ELSE.
      vl_flag = abap_on.

    ENDIF.

    CHECK NOT vl_flag IS INITIAL.
* Verifica se tem permissão para executar a opção da tela.
    PERFORM zf_verify_authority_check USING vl_direcao
                                            vl_area
                                            vl_bukrs
                                            vl_vkbur.

    CLEAR vl_flag.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_limit_select_option
*&---------------------------------------------------------------------*
*& Restringe as opções da tela de seleção
*&---------------------------------------------------------------------*
FORM zf_limit_select_option.

  TYPE-POOLS sscr. "Tipo da tela de selação
* Restringe os dados do parâmetro da tela de seleção
  DATA: tl_screen TYPE sscr_restrict,  "Tabelda tela de seleção
* Estruturas para preencher a tab. tl_screen
        el_opts   TYPE sscr_opt_list, "Estrutura da restrição da lista de opções
        el_assoc  TYPE sscr_ass.      "Estrutura da lista do nome da variável restringida

  CONSTANTS: cl_objectkey1(10) TYPE c VALUE 'OBJECTKEY1',
             cl_objectkey2(10) TYPE c VALUE 'OBJECTKEY2',
             cl_objectkey3(10) TYPE c VALUE 'OBJECTKEY3'.

*** Insumos/Vendas - 1000.
* Restringe o campo "Escritório de vendas"  selection para somente EQ.
  el_opts-name       = cl_objectkey1.
  el_opts-options-eq = abap_on.
  APPEND el_opts TO tl_screen-opt_list_tab.
* Carrega informações para determinar o campo que sofrerá a restrição na tela de
* seleção.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_VKBUR'.
  el_assoc-sg_main   = sy-abcde+8(1).  "I
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey1.
  APPEND el_assoc TO tl_screen-ass_tab.
*** Insumos/Compra - 1000
* Restringe o campo "Empresa"  selection para somente EQ.
  el_opts-name       = cl_objectkey2.
  el_opts-options-eq = abap_on.
  APPEND el_opts TO tl_screen-opt_list_tab.
* Carrega informações para determinar o campo que sofrerá a restrição na tela de
* seleção.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_BUKRS'.
  el_assoc-sg_main   = sy-abcde+8(1).  "I
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey2.
  APPEND el_assoc TO tl_screen-ass_tab.
*** Insumos/Compra - 1000
* Restringe o campo "Safra"  selection para somente EQ.
  el_opts-name       = cl_objectkey3.
  el_opts-options-eq = abap_on.
  APPEND el_opts TO tl_screen-opt_list_tab.
* Carrega informações para determinar o campo que sofrerá a restrição na tela de
* seleção.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_SAFR2'.
  el_assoc-sg_main   = sy-abcde+8(1).  "I
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey3.
  APPEND el_assoc TO tl_screen-ass_tab.
* Função para restringir Selection  Option
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = tl_screen
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.
* Verifica de função executou com erro.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.
