*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 29/08/2009                                              &*
*& Descrição: Relatório ALV Controle de Dados Imóveis/Hipotecas       &*
*& Transação: ZAA03                                                   &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK906388   29.08.2009                            &*
*&--------------------------------------------------------------------&*

REPORT  zaa02 MESSAGE-ID tree_control_msg.

*&---------------------------------------------------------------------*
*& ESTRUTURA
*&---------------------------------------------------------------------*

TABLES: anla.

TYPES: BEGIN OF ty_zaa_controle_doc,
  bukrs       TYPE zaa_controle_doc-bukrs,
  anln1       TYPE zaa_controle_doc-anln1,
  anln2       TYPE zaa_controle_doc-anln2,
  endereco    TYPE zaa_controle_doc-endereco,
  bairro      TYPE zaa_controle_doc-bairro,
  municipio   TYPE zaa_controle_doc-municipio,
  estado      TYPE zaa_controle_doc-estado,
  pais        TYPE zaa_controle_doc-pais,
  matricula   TYPE zaa_controle_doc-matricula,
  area        TYPE zaa_controle_doc-area,
  feins       TYPE zaa_controle_doc-feins,
  cartorio    TYPE zaa_controle_doc-cartorio,
  comarca     TYPE zaa_controle_doc-comarca,
  estado_com  TYPE zaa_controle_doc-estado_com,
  ccir        TYPE zaa_controle_doc-ccir,
  texto       TYPE zaa_controle_doc-texto,
  usuario     TYPE zaa_controle_doc-usuario,
  data_entr   TYPE zaa_controle_doc-data_entr,
  hora_entr   TYPE zaa_controle_doc-hora_entr,
  data_mod    TYPE zaa_controle_doc-data_mod,
  hora_mod    TYPE zaa_controle_doc-hora_mod,
       END OF ty_zaa_controle_doc,

       BEGIN OF ty_zaa_controle_hip,
  bukrs       TYPE zaa_controle_hip-bukrs,
  anln1       TYPE zaa_controle_hip-anln1,
  buzei       TYPE zaa_controle_hip-buzei,
  credor      TYPE zaa_controle_hip-credor,
  grau        TYPE zaa_controle_hip-grau,
  operacao    TYPE zaa_controle_hip-operacao,
  dmbtr       TYPE zaa_controle_hip-dmbtr,
  waers       TYPE zaa_controle_hip-waers,
  contrato    TYPE zaa_controle_hip-contrato,
  vencimento  TYPE zaa_controle_hip-vencimento,
  assinatura  TYPE zaa_controle_hip-assinatura,
  flag        TYPE zaa_controle_hip-flag,
       END OF ty_zaa_controle_hip,

       BEGIN OF ty_anla,
  bukrs       TYPE anla-bukrs,
  anln1       TYPE anla-anln1,
  anln2       TYPE anla-anln2,
  txt50       TYPE anla-txt50,
  txa50       TYPE anla-txa50,
       END OF ty_anla,

       BEGIN OF ty_anlh,
  bukrs       TYPE anlh-bukrs,
  anln1       TYPE anlh-anln1,
  anlhtxt     TYPE anlh-anlhtxt,
       END OF ty_anlh,

       BEGIN OF ty_saida,
  bukrs       TYPE zaa_controle_doc-bukrs,
  anln1       TYPE zaa_controle_doc-anln1,
  anln2       TYPE zaa_controle_doc-anln2,
  txt50(100),
*  TXT50       TYPE ANLA-TXT50,
*  TXA50       TYPE ANLA-TXA50,
  anlhtxt     TYPE anlh-anlhtxt,
  endereco    TYPE zaa_controle_doc-endereco,
  bairro      TYPE zaa_controle_doc-bairro,
  municipio   TYPE zaa_controle_doc-municipio,
  estado      TYPE zaa_controle_doc-estado,
  pais        TYPE zaa_controle_doc-pais,
  matricula   TYPE zaa_controle_doc-matricula,
  area        TYPE zaa_controle_doc-area,
  feins       TYPE zaa_controle_doc-feins,
  cartorio    TYPE zaa_controle_doc-cartorio,
  comarca     TYPE zaa_controle_doc-comarca,
  estado_com  TYPE zaa_controle_doc-estado_com,
  ccir        TYPE zaa_controle_doc-ccir,
  texto       TYPE zaa_controle_doc-texto,
  usuario     TYPE zaa_controle_doc-usuario,
  data_entr   TYPE zaa_controle_doc-data_entr,
  hora_entr   TYPE zaa_controle_doc-hora_entr,
  data_mod    TYPE zaa_controle_doc-data_mod,
  hora_mod    TYPE zaa_controle_doc-hora_mod,
  buzei       TYPE zaa_controle_hip-buzei,
  credor      TYPE zaa_controle_hip-credor,
  grau        TYPE zaa_controle_hip-grau,
  operacao    TYPE zaa_controle_hip-operacao,
  dmbtr       TYPE zaa_controle_hip-dmbtr,
  waers       TYPE zaa_controle_hip-waers,
  contrato    TYPE zaa_controle_hip-contrato,
  vencimento  TYPE zaa_controle_hip-vencimento,
  assinatura  TYPE zaa_controle_hip-assinatura,
  flag        TYPE zaa_controle_hip-flag,
       END OF ty_saida.

*&---------------------------------------------------------------------*
*& Definições para ALV
*&---------------------------------------------------------------------*

TYPE-POOLS: slis,
            kkblo.



DATA: repid           LIKE sy-repid.
DATA: fieldcat        TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: layout          TYPE slis_layout_alv.
DATA: print           TYPE slis_print_alv.
DATA: sort            TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      events          TYPE slis_t_event, "evento HOTSPOT
      xs_events       TYPE slis_alv_event. "evento para HotSpot
DATA: variante        LIKE disvariant,
      def_variante    LIKE disvariant.
DATA: w_tit(70).


*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: t_zaa_controle_doc TYPE TABLE OF ty_zaa_controle_doc,
      t_zaa_controle_hip TYPE TABLE OF ty_zaa_controle_hip,
      t_anla             TYPE TABLE OF ty_anla,
      t_anlh             TYPE TABLE OF ty_anlh,
      t_saida            TYPE TABLE OF ty_saida.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA: wa_zaa_controle_doc TYPE ty_zaa_controle_doc,
      wa_zaa_controle_hip TYPE ty_zaa_controle_hip,
      wa_anla             TYPE ty_anla,
      wa_anlh             TYPE ty_anlh,
      wa_saida            TYPE ty_saida.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇAO.
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: s_bukrs FOR wa_zaa_controle_doc-bukrs OBLIGATORY,
                s_anln1 FOR anla-anln1 ,
                s_anln2 FOR wa_zaa_controle_doc-anln2.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETERS: p_varia LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
DATA: vg_repid           LIKE sy-repid,
      vg_variant        TYPE disvariant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  variante-report = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH text-m01 DISPLAY LIKE 'E'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
  ENDIF.


START-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Iniciar variaveis do ALV
*&---------------------------------------------------------------------*

  PERFORM alv_init.

*&---------------------------------------------------------------------*
*& START OF SELECTION.
*&---------------------------------------------------------------------*

  PERFORM seleciona_dados.

*&---------------------------------------------------------------------*
*&ORGANIZAR DADOS.
*&---------------------------------------------------------------------*

  LOOP AT t_zaa_controle_hip INTO wa_zaa_controle_hip.
    READ TABLE t_zaa_controle_doc  INTO wa_zaa_controle_doc
          WITH KEY bukrs = wa_zaa_controle_hip-bukrs
                   anln1 = wa_zaa_controle_hip-anln1.
    IF sy-subrc IS INITIAL.
    ENDIF.
    READ TABLE t_anla INTO wa_anla WITH KEY anln1 = wa_zaa_controle_hip-anln1.
    IF sy-subrc IS INITIAL.
    ENDIF.
    READ TABLE t_anlh INTO wa_anlh WITH KEY anln1 = wa_anla-anln1.
    wa_saida-bukrs       = wa_zaa_controle_doc-bukrs.
    wa_saida-anln1       = wa_zaa_controle_doc-anln1.
    wa_saida-anln2       = wa_zaa_controle_doc-anln2.
    CONCATENATE wa_anla-txt50 wa_anla-txa50 INTO wa_saida-txt50
                SEPARATED BY ' - '.
    wa_saida-anlhtxt     = wa_anlh-anlhtxt.
    wa_saida-endereco    = wa_zaa_controle_doc-endereco.
    wa_saida-bairro      = wa_zaa_controle_doc-bairro.
    wa_saida-municipio   = wa_zaa_controle_doc-municipio.
    wa_saida-estado      = wa_zaa_controle_doc-estado.
    wa_saida-pais        = wa_zaa_controle_doc-pais.
    wa_saida-matricula   = wa_zaa_controle_doc-matricula.
    wa_saida-area        = wa_zaa_controle_doc-area.
    wa_saida-feins       = wa_zaa_controle_doc-feins.
    wa_saida-cartorio    = wa_zaa_controle_doc-cartorio.
    wa_saida-comarca     = wa_zaa_controle_doc-comarca.
    wa_saida-estado_com  = wa_zaa_controle_doc-estado_com.
    wa_saida-ccir        = wa_zaa_controle_doc-ccir.
    wa_saida-texto       = wa_zaa_controle_doc-texto.
    wa_saida-usuario     = wa_zaa_controle_doc-usuario.
    wa_saida-data_entr   = wa_zaa_controle_doc-data_entr.
    wa_saida-hora_entr   = wa_zaa_controle_doc-hora_entr.
    wa_saida-usuario     = wa_zaa_controle_doc-usuario.
    wa_saida-bukrs       = wa_zaa_controle_hip-bukrs.
    wa_saida-anln1       = wa_zaa_controle_hip-anln1.
    wa_saida-buzei       = wa_zaa_controle_hip-buzei.
    wa_saida-credor      = wa_zaa_controle_hip-credor.
    wa_saida-grau        = wa_zaa_controle_hip-grau.
    wa_saida-operacao    = wa_zaa_controle_hip-operacao.
    wa_saida-dmbtr       = wa_zaa_controle_hip-dmbtr.
    wa_saida-waers       = wa_zaa_controle_hip-waers.
    wa_saida-contrato    = wa_zaa_controle_hip-contrato.
    wa_saida-vencimento  = wa_zaa_controle_hip-vencimento.
    APPEND wa_saida TO t_saida.
  ENDLOOP.
  LOOP AT t_saida INTO wa_saida.
  ENDLOOP.
  PERFORM f_alv_sort.
*&---------------------------------------------------------------------*
*& CHAMAR ALV.
*&---------------------------------------------------------------------*

  PERFORM chama_alv.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*

FORM seleciona_dados .

  SELECT bukrs anln1 anln2 endereco bairro municipio estado pais matricula area feins cartorio comarca estado_com ccir texto usuario data_entr hora_entr
    FROM zaa_controle_doc
    INTO TABLE t_zaa_controle_doc
    WHERE bukrs IN s_bukrs
    AND anln1 IN s_anln1
    AND anln2 IN s_anln2.
  IF sy-subrc IS INITIAL.
  ENDIF.
  SELECT bukrs anln1 buzei credor grau operacao dmbtr waers contrato vencimento
    FROM zaa_controle_hip
    INTO TABLE t_zaa_controle_hip
    FOR ALL ENTRIES IN t_zaa_controle_doc
    WHERE bukrs EQ t_zaa_controle_doc-bukrs
    AND anln1 EQ t_zaa_controle_doc-anln1.
  IF sy-subrc IS INITIAL.
  ENDIF.
  SELECT bukrs anln1 anln2 txt50 txa50
    FROM anla
    INTO TABLE t_anla
    FOR ALL ENTRIES IN t_zaa_controle_hip
*      WHERE BURKS EQ T_ZAA_CONTROLE_HIP-BURKS
    WHERE anln1 EQ t_zaa_controle_hip-anln1.
  IF sy-subrc IS INITIAL.
  ENDIF.
  SELECT bukrs anln1 anlhtxt
    FROM anlh
    INTO TABLE t_anlh
    FOR ALL ENTRIES IN t_anla
    WHERE anln1 EQ t_anla-anln1.

ENDFORM.                    " SELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  ALV_INIT
*&---------------------------------------------------------------------*

FORM alv_init .
  CLEAR: variante.
  repid = sy-repid.

  variante-report = repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = variante
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_varia = variante-variant.
  ENDIF.
ENDFORM.                    " ALV_INIT
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chama_alv .
*  FIELDCAT-JUST          = X_JUST  (X)
  PERFORM definir_eventos. "evento para HotSpot
  REFRESH: fieldcat.

*  BREAK-POINT.
  PERFORM monta_fieldcat USING:
      'BUKRS'      'T_SAIDA' 'ZAA_CONTROLE_DOC' '             ' ' ' ' ' ' ' ' ' 'X',
      'ANLN1'      'T_SAIDA' 'ZAA_CONTROLE_DOC' '             ' ' ' ' ' ' ' 'X' 'X',
      'ANLN2'      'T_SAIDA' 'ZAA_CONTROLE_DOC' 'Sub.nº.      ' ' ' ' ' ' ' ' ' 'X',
      'TXT50'      'T_SAIDA' 'ANLA            ' 'Denominação  ' ' ' ' ' ' ' ' ' ' ',
*      'TXA50'      'T_SAIDA' 'ANLA            ' 'Denominação  ' ' ' ' ' ' ' ' ' ' ',
      'ANLHTXT'    'T_SAIDA' 'ANLH            ' '             ' ' ' ' ' ' ' ' ' ' ',
      'ENDERECO'   'T_SAIDA' 'ZAA_CONTROLE_DOC' '             ' ' ' ' ' ' ' ' ' ' ',
      'BAIRRO'     'T_SAIDA' 'ZAA_CONTROLE_DOC' '             ' ' ' ' ' ' ' ' ' ' ',
      'MUNICIPIO'  'T_SAIDA' 'ZAA_CONTROLE_DOC' '             ' ' ' ' ' ' ' ' ' ' ',
      'ESTADO'     'T_SAIDA' 'ZAA_CONTROLE_DOC' 'Estado       ' ' ' ' ' ' ' ' ' ' ',
      'PAIS'       'T_SAIDA' 'ZAA_CONTROLE_DOC' '             ' ' ' ' ' ' ' ' ' ' ',
      'MATRICULA'  'T_SAIDA' 'ZAA_CONTROLE_DOC' 'Matricula    ' ' ' ' ' ' ' ' ' ' ',
      'AREA'       'T_SAIDA' 'ZAA_CONTROLE_DOC' 'Área         ' ' ' ' ' ' ' ' ' ' ',
      'FEINS'      'T_SAIDA' 'ZAA_CONTROLE_DOC' 'Unidade Terra' ' ' ' ' ' ' ' ' ' ',
      'CARTORIO'   'T_SAIDA' 'ZAA_CONTROLE_DOC' 'Cartório     ' ' ' ' ' ' ' ' ' ' ',
      'COMARCA'    'T_SAIDA' 'ZAA_CONTROLE_DOC' 'Comarca      ' ' ' ' ' ' ' ' ' ' ',
      'ESTADO_COM' 'T_SAIDA' 'ZAA_CONTROLE_DOC' 'Estado       ' ' ' ' ' ' ' ' ' ' ',
      'CCIR'       'T_SAIDA' 'ZAA_CONTROLE_DOC' 'CCIR         ' ' ' ' ' ' ' ' ' ' ',
      'BUZEI'      'T_SAIDA' 'ZAA_CONTROLE_HIP' 'Seq.Hip.     ' ' ' ' ' ' ' ' ' ' ',
      'CREDOR'     'T_SAIDA' 'ZAA_CONTROLE_HIP' 'Credor       ' ' ' ' ' ' ' ' ' ' ',
      'GRAU'       'T_SAIDA' 'ZAA_CONTROLE_HIP' ' Grau        ' ' ' ' ' ' ' ' ' ' ',
      'OPERACAO'   'T_SAIDA' 'ZAA_CONTROLE_HIP' 'Operação     ' ' ' ' ' ' ' ' ' ' ',
      'DMBTR'      'T_SAIDA' 'ZAA_CONTROLE_HIP' 'Valor        ' ' ' ' ' ' ' ' ' ' ',
      'WAERS'      'T_SAIDA' 'ZAA_CONTROLE_HIP' '             ' ' ' ' ' ' ' ' ' ' ',
      'CONTRATO'   'T_SAIDA' 'ZAA_CONTROLE_HIP' '             ' ' ' ' ' ' ' ' ' ' ',
      'ASSINATURA' 'T_SAIDA' 'ZAA_CONTROLE_HIP' 'Assinatura' ' ' ' ' ' ' ' ' ' ',
      'VENCIMENTO' 'T_SAIDA' 'ZAA_CONTROLE_HIP' 'Vencimento' ' ' ' ' ' ' ' ' ' '.


  w_tit ='Relatório Controle de Dados de Imóveis/Hipotecas'.

  layout-zebra = 'X'.
  print-no_print_listinfos = 'X'.
  variante-variant = p_varia.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     i_callback_program                = repid
*  I_CALLBACK_PF_STATUS_SET          = 'SET_STATUS'
*  i_callback_user_command           = 'COMANDO'
     it_fieldcat                       = fieldcat[]
     it_sort                           = sort[]
     is_layout                         = layout
     i_grid_title                      = w_tit
     i_default                         = 'X'
     i_save                            = 'A'
     it_events                         = events "evento para HotSpot
     is_variant                        = variante
     is_print                          = print
    TABLES
      t_outtab                          = t_saida
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " CHAMA_ALV

*----------------------------------------------------------------------*
FORM monta_fieldcat USING
               x_field x_tab x_ref x_text x_sum x_just x_qfield
               x_hotspot x_key.
*----------------------------------------------------------------------*
*

  fieldcat-fieldname     = x_field.
  fieldcat-tabname       = x_tab.
  fieldcat-ref_tabname   = x_ref.
  fieldcat-do_sum        = x_sum.
  fieldcat-just          = x_just.
  fieldcat-qfieldname    = x_qfield.
  fieldcat-hotspot       = x_hotspot.
  fieldcat-key           = x_key.
  fieldcat-seltext_l     =
  fieldcat-seltext_m     =
  fieldcat-seltext_s     =
  fieldcat-reptext_ddic  = x_text.
  APPEND fieldcat.
  CLEAR fieldcat.
*
ENDFORM.                               " MONTA_FIELDCAT

*---------------------------------------------------------------------*
*            USER_COMMAND                                             *
*---------------------------------------------------------------------*
*FORM comando USING ucomm LIKE sy-ucomm
*                        selfield TYPE slis_selfield.
*
*
*ENDFORM.                                                    "COMANDO

*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
FORM set_status USING pf_tab TYPE slis_t_extab.


ENDFORM.                    "SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SUB_TOTAIS
*&---------------------------------------------------------------------*

FORM f_alv_sort .
  CLEAR sort.
*
*  BREAK-POINT.
  sort-fieldname = 'BUKRS'.      sort-spos = 1.  APPEND sort.
  sort-fieldname = 'ANLN1'.      sort-spos = 2.  APPEND sort.
  sort-fieldname = 'ANLN2'.      sort-spos = 3.  APPEND sort.
  sort-fieldname = 'TXT50'.      sort-spos = 4.  APPEND sort.
  sort-fieldname = 'ANLHTXT'.    sort-spos = 5.  APPEND sort.
  sort-fieldname = 'ENDERECO'.   sort-spos = 6.  APPEND sort.
  sort-fieldname = 'BAIRRO'.     sort-spos = 7.  APPEND sort.
  sort-fieldname = 'MUNICIPIO'.  sort-spos = 8.  APPEND sort.
  sort-fieldname = 'ESTADO'.     sort-spos = 9.  APPEND sort.
  sort-fieldname = 'PAIS'.       sort-spos = 10. APPEND sort.
  sort-fieldname = 'MATRICULA'.  sort-spos = 11. APPEND sort.
  sort-fieldname = 'AREA'.       sort-spos = 12. APPEND sort.
  sort-fieldname = 'FEINS'.      sort-spos = 13. APPEND sort.
  sort-fieldname = 'CARTORIO'.   sort-spos = 14. APPEND sort.
  sort-fieldname = 'COMARCA'.    sort-spos = 15. APPEND sort.
  sort-fieldname = 'ESTADO_COM'. sort-spos = 16. APPEND sort.
  sort-fieldname = 'CCIR'.       sort-spos = 17. APPEND sort.
**
ENDFORM.                    " F_ALV_SORT

*---------------------------------------------------------------------*
*       FORM COMANDO                                            *
*---------------------------------------------------------------------*
*       ........"evento para HotSpot                                                      *
*---------------------------------------------------------------------*
FORM comando USING ucomm LIKE sy-ucomm
                         selfield TYPE kkblo_selfield.
  selfield = selfield.                                      "#EC CALLED
  CASE ucomm.
    WHEN '&IC1'.
      READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.

      IF sy-subrc EQ 0.
* Se foi clicado na coluna EBELN.
        IF selfield-fieldname = 'ANLN1'.
* Passa o valor clicado na coluna como parâmetro para a transação
          SET PARAMETER ID 'AN1' FIELD wa_saida-anln1.
          SET PARAMETER ID 'AN2' FIELD wa_saida-anln2.
          SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.

          SET PARAMETER ID 'MEN' FIELD sy-repid.
* Chamo a transação
          CALL TRANSACTION 'ZAA02' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM. "COMANDO
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text "evento para HotSpot
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.
  PERFORM f_carregar_eventos USING:
                                 slis_ev_user_command 'COMANDO'.
*                                 SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text "evento para HotSpot
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos
