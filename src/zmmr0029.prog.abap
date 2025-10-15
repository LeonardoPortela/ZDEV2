*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Sobral                                             &*
*& Data.....: 13/06/2013                                              &*
*& Descrição: Disponibilidade de Fardos                               &*
*& Transação: ZMM0051                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&                                                                    &*
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Report  ZMMR0029
*&---------------------------------------------------------------------*
REPORT  zmmr0029.

** TABLES
**----------------------------------------------------------------------
TABLES: mchb, mara,    "STANDARD
        zmmt0025.

** CONSTANTS
**----------------------------------------------------------------------
CONSTANTS:  c_x              TYPE c   VALUE 'X',
            c_a(1)           TYPE c   VALUE 'A',
            c_c(1)           TYPE c   VALUE 'C'.

** TYPES
**----------------------------------------------------------------------
TYPES:  BEGIN OF ty_saida,
*          mark,
          material        TYPE c LENGTH 30,
          matnr           TYPE mara-matnr,      "Material
          normt           TYPE mara-normt,      "Tipo
          werks           TYPE mchb-werks,      "Centro
          name4           type lfa1-name4,
          "Estoque em Fardos
          est_fisico_f    TYPE mchb-clabs,      "Estoque Fisico
          reservado_f     TYPE mchb-cspem,      "Reservado
          disp_venda_f    TYPE mchb-clabs,      "Disponivel pra venda
          micro_alto_f    TYPE mchb-clabs,      "Micro Bom
          micro_baixo_f   TYPE mchb-clabs,      "Micro Baixo
          "Estoque em Kilos
          est_fisico_k    TYPE mchb-clabs,      "Estoque Fisico
          reservado_k     TYPE mchb-cspem,      "Reservado
          disp_venda_k    TYPE mchb-clabs,      "Disponivel pra venda
          micro_alto_k    TYPE mchb-clabs,      "Micro Bom
          micro_baixo_k   TYPE mchb-clabs,      "Micro Baixo
        END OF ty_saida,

        BEGIN OF ty_mara,
          matnr   TYPE mara-matnr,
          matkl   TYPE mara-matkl,
          normt   TYPE mara-normt,
        END OF ty_mara,

        BEGIN OF ty_lfa1,
          lifnr     type lfa1-lifnr,
          NAME4     type LFA1-NAME4,
        END OF ty_lfa1,

        BEGIN OF ty_mchb,
          matnr   TYPE mchb-matnr,
          werks   TYPE mchb-werks,
          charg   TYPE mchb-charg,
          clabs   TYPE mchb-clabs,
          cspem   TYPE mchb-cspem,
          lifnr   type lfa1-lifnr,
        END OF ty_mchb.

** INTERNAL TABLES
**----------------------------------------------------------------------
DATA: it_zmmt0025     TYPE TABLE OF zmmt0025,
      it_saida        TYPE TABLE OF ty_saida,
      it_saida_aux    TYPE TABLE OF ty_saida,
      it_mara         TYPE TABLE OF ty_mara,
      it_mchb         TYPE TABLE OF ty_mchb,
      it_matnr        TYPE TABLE OF zmme_cl,
      it_lfa1         TYPE TABLE OF ty_lfa1,
      it_return       TYPE TABLE OF zmme_cl,
      it_return_aux   TYPE TABLE OF zmme_cl.

** WORK AREAS
**----------------------------------------------------------------------
DATA: wa_zmmt0025     TYPE zmmt0025,
      wa_saida        TYPE ty_saida,
      wa_saida_aux    TYPE ty_saida,
      wa_mara         TYPE ty_mara,
      wa_mchb         TYPE ty_mchb,
      wa_lfa1         TYPE ty_lfa1,
      wa_return       TYPE zmme_cl.

** VARIABLES
**----------------------------------------------------------------------

** Tipos standard
**----------------------------------------------------------------------
TYPE-POOLS: slis.    "Tipos globais para ALV

DATA: sort            TYPE slis_t_sortinfo_alv WITH HEADER LINE.

** Tabelas internas ALV
**----------------------------------------------------------------------
DATA:
  linecolor           TYPE slis_specialcol_alv   OCCURS 0 WITH HEADER LINE,
  listheader          TYPE slis_t_listheader,
  fieldcat            TYPE slis_t_fieldcat_alv            WITH HEADER LINE.
*  t_sort              TYPE slis_sortinfo_alv     OCCURS 0 WITH HEADER LINE.

DATA:
*  listheader          TYPE slis_listheader,   "Cabeçalho
  layout              TYPE slis_layout_alv,     "layout para saída
  variante            LIKE disvariant,          "Variante de exibição
  def_variante        LIKE disvariant,
  print               TYPE slis_print_alv,
  repid               LIKE sy-repid.

** SELECTION SCREEN
**----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK one WITH FRAME TITLE text-t01.

SELECT-OPTIONS:
  s_werks   FOR mchb-werks      OBLIGATORY                NO INTERVALS,
  s_matkl   FOR mara-matkl                  NO-EXTENSION  NO INTERVALS,
  s_matnr   FOR mara-matnr                                NO INTERVALS,
  s_safra   FOR zmmt0025-atinn  OBLIGATORY  NO-EXTENSION  NO INTERVALS.

SELECTION-SCREEN END OF BLOCK one.

START-OF-SELECTION.
  PERFORM:  f_validar_parametros,
            f_seleciona_dados,
            f_sort,
            f_relatorio_alv.



*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_PARAMETROS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_validar_parametros.
  IF    s_matnr IS INITIAL AND s_matkl IS INITIAL
    OR  s_matnr IS NOT INITIAL AND s_matkl IS NOT INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar material' 'ou grupo de material'.
  ENDIF.
ENDFORM.                    " F_VALIDAR_PARAMETROS

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_seleciona_dados.
  SELECT * FROM zmmt0025 INTO TABLE it_zmmt0025.

  CHECK it_zmmt0025 IS NOT INITIAL.

  SELECT matnr matkl normt FROM mara
    INTO TABLE it_mara
  WHERE matnr IN s_matnr
    AND matkl IN s_matkl.

  CHECK it_mara IS NOT INITIAL.
  SORT it_mara BY matnr.

  SELECT matnr werks charg clabs cspem FROM mchb
    INTO TABLE it_mchb
    FOR ALL ENTRIES IN it_mara
  WHERE werks IN s_werks
    AND matnr EQ it_mara-matnr
    AND ( clabs > 0 OR cspem > 0 ).

  CHECK it_mchb IS NOT INITIAL.
  SORT it_mchb BY matnr werks charg.

  LOOP AT it_mchb INTO wa_mchb.
    MOVE: wa_mchb-matnr TO wa_return-matnr,
          wa_mchb-charg TO wa_return-charg.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = wa_mchb-werks
          importing
            output = wa_mchb-lifnr.
    MODIFY it_mchb from wa_mchb INDEX sy-tabix TRANSPORTING lifnr.
    READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA'.
    wa_return-atinn = wa_zmmt0025-atinn.
    APPEND wa_return TO it_matnr.

    READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'MIC'.
    wa_return-atinn = wa_zmmt0025-atinn.
    APPEND wa_return TO it_matnr.
  ENDLOOP.

  select lifnr name4
    from lfa1
    into TABLE it_lfa1
    FOR ALL ENTRIES IN it_mchb
    where lifnr = it_mchb-lifnr.

  IF it_matnr IS NOT INITIAL.
    CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
      TABLES
        t_matnr  = it_matnr
        t_return = it_return.
*    EXCEPTIONS
*        ERRO4          = 1
*        OTHERS         = 2.
    IF sy-subrc <> 0.
    ELSE.
      CLEAR: wa_zmmt0025.

      READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA'.
      IF wa_zmmt0025 IS NOT INITIAL.
        LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
*                                           AND atwrt NE s_safra-low.
                                           AND ( atwrt NE s_safra-low OR atwrt = '' ).
          APPEND wa_return TO it_return_aux.
          CLEAR: wa_return.
        ENDLOOP.
        " Deletar com SAFRA diferente da informada
        IF it_return_aux[] IS NOT INITIAL.
          LOOP AT it_return_aux INTO wa_return.
            DELETE it_return WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
            DELETE it_mchb   WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK it_mchb IS NOT INITIAL AND it_return IS NOT INITIAL.
  SORT: it_mchb   BY matnr werks charg,
        it_return BY matnr charg atinn,
        it_lfa1   by lifnr.

  DATA: vl_alto   TYPE zmme_cl-atwrt VALUE '3.50',
        vl_baixo  TYPE zmme_cl-atwrt VALUE '3.49',
        vl_matnr  TYPE c LENGTH 40. "*---> 14/06/2023 - Migração S4 - JS

  READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'MIC'.

  LOOP AT it_mchb INTO wa_mchb.
    wa_saida-matnr = wa_mchb-matnr.
    SHIFT wa_saida-matnr LEFT DELETING LEADING '0'.
    wa_saida-werks = wa_mchb-werks.

    READ TABLE it_lfa1 into wa_lfa1 with key lifnr = wa_mchb-lifnr BINARY SEARCH.
    if sy-subrc = 0.
       wa_saida-name4 = wa_lfa1-name4.
    endif.

    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mchb-matnr BINARY SEARCH.
    wa_saida-normt = wa_mara-normt.

     vl_matnr = wa_mchb-matnr.

    SHIFT vl_matnr LEFT DELETING LEADING '0'.
    CONCATENATE vl_matnr '/' wa_mara-normt INTO wa_saida-material SEPARATED BY space.

    READ TABLE it_return INTO wa_return WITH KEY matnr = wa_mchb-matnr
                                                 charg = wa_mchb-charg
                                                 atinn = wa_zmmt0025-atinn BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-est_fisico_k     = wa_mchb-clabs + wa_mchb-cspem.
      IF wa_saida-est_fisico_k > 0.
        wa_saida-est_fisico_f   = 1.
      ENDIF.

      IF wa_mchb-cspem > 0.
        wa_saida-reservado_k    = wa_mchb-cspem.
        wa_saida-reservado_f    = 1.
      ENDIF.

      IF wa_mchb-clabs > 0.
        wa_saida-disp_venda_k   = wa_mchb-clabs.
        wa_saida-disp_venda_f   = 1.
      ENDIF.

      IF wa_return-atwrt >= vl_alto AND wa_mchb-clabs > 0.
        wa_saida-micro_alto_k  = wa_mchb-clabs.
        wa_saida-micro_alto_f  = 1.
      ENDIF.

      IF wa_return-atwrt <= vl_baixo AND wa_mchb-clabs > 0.
        wa_saida-micro_baixo_k  = wa_mchb-clabs.
        wa_saida-micro_baixo_f  = 1.
      ENDIF.

*    APPEND wa_saida TO it_saida.
      COLLECT wa_saida INTO it_saida.
    ENDIF.

    CLEAR: wa_saida, wa_mchb, wa_mara.
  ENDLOOP.

  CHECK it_saida IS NOT INITIAL.
ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_SORT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_sort.
  PERFORM:
*          f_organiza USING 1  'MATERIAL'      'IT_SAIDA'    'C'   'X'   'X',
          f_organiza USING 1  'NORMT'         'IT_SAIDA'    'C'   'X'   'X',
          f_organiza USING 2  'MATNR'         'IT_SAIDA'    'C'   ' '   ' ',
          f_organiza USING 3  'WERKS'         'IT_SAIDA'    'C'   ' '   ' '.
ENDFORM.                    " F_SORT

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_organiza  USING p_num p_field p_table p_group p_subtot p_expa.

  sort-spos       = p_num.
  sort-fieldname  = p_field.
  sort-tabname    = p_table.
  sort-group      = p_group.    "c_c.
  sort-subtot     = p_subtot.
  sort-expa       = p_expa.     "c_x
  APPEND sort.
ENDFORM.                    " F_ORGANIZA

*&---------------------------------------------------------------------*
*&      Form  F_RELATORIO_ALV
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_relatorio_alv.
  REFRESH: fieldcat.

  PERFORM f_monta_fieldcat USING:
*      'MATERIAL'      'IT_SAIDA'    ''      'Material / Tipo     '  ' '  'X'  ' '  ' '  '18',
      'NORMT'         'IT_SAIDA'    'MARA'  'Tipo                '  ' '  'X'  ' '  ' '  '06',
      'MATNR'         'IT_SAIDA'    'MARA'  'Material            '  ' '  'X'  ' '  ' '  '18',
      'WERKS'         'IT_SAIDA'    'MCHB'  'Centro              '  ' '  'X'  ' '  ' '  '04',
      'NAME4'         'IT_SAIDA'    ''      'Nome Centro         '  ' '  'X'  ' '  ' '  '25',
      'EST_FISICO_K'  'IT_SAIDA'    ''      'Est. Fisico (KG)    '  ' '  'X'  ' '  'X'  '14',       "MCHB
      'RESERVADO_K'   'IT_SAIDA'    ''      'Reservado (KG)      '  ' '  'X'  ' '  'X'  '14',       "MCHB
      'DISP_VENDA_K'  'IT_SAIDA'    ''      'Disp. Venda (KG)    '  ' '  'X'  ' '  'X'  '14',       "MCHB
      'MICRO_ALTO_K'  'IT_SAIDA'    ''      'Micro Alto (KG)     '  ' '  'X'  ' '  'X'  '14',       "MCHB
      'MICRO_BAIXO_K' 'IT_SAIDA'    ''      'Micro Baixo (KG)    '  ' '  'X'  ' '  'X'  '14',       "MCHB
      'EST_FISICO_F'  'IT_SAIDA'    ''      'Est. Fisico (Fardos)'  ' '  'X'  ' '  'X'  '14',       "MCHB
      'RESERVADO_F'   'IT_SAIDA'    ''      'Reservado (Fardos)  '  ' '  'X'  ' '  'X'  '14',       "MCHB
      'DISP_VENDA_F'  'IT_SAIDA'    ''      'Disp. Venda (Fardos)'  ' '  'X'  ' '  'X'  '14',       "MCHB
      'MICRO_ALTO_F'  'IT_SAIDA'    ''      'Micro Alto (Fardos) '  ' '  'X'  ' '  'X'  '14',       "MCHB
      'MICRO_BAIXO_F' 'IT_SAIDA'    ''      'Micro Baixo (Fardos)'  ' '  'X'  ' '  'X'  '14'.       "MCHB

  layout-zebra              = c_x.
  layout-colwidth_optimize  = c_x.
  layout-totals_only        = c_x.
  print-no_print_listinfos  = c_x.

  repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = repid
*      i_callback_user_command = 'COMANDO'
      it_fieldcat             = fieldcat[]
      it_sort                 = sort[]
      is_layout               = layout
      i_default               = 'X'
      i_save                  = c_a
      is_variant              = variante
      is_print                = print
    TABLES
      t_outtab                = it_saida
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " F_RELATORIO_ALV

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_monta_fieldcat  USING x_field x_tab x_ref x_text x_hotspot x_just x_qfield x_sum x_outputlen.
**----------------------------------------------------------------------*

  fieldcat-fieldname     = x_field.
  fieldcat-tabname       = x_tab.
  fieldcat-ref_tabname   = x_ref.
  fieldcat-reptext_ddic  = x_text.
  fieldcat-hotspot       = x_hotspot.
  fieldcat-just          = x_just.
  fieldcat-qfieldname    = x_qfield.
  fieldcat-do_sum        = x_sum.
  fieldcat-outputlen     = x_outputlen.

  APPEND fieldcat.
  CLEAR fieldcat.
ENDFORM.                    " F_MONTA_FIELDCAT
