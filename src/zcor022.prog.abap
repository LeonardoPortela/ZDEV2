*&---------------------------------------------------------------------*
*& Report  ZCOR022
*&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Atualização do Ciclo  de Distribuição - KSV2            *
* Transação..: ZCOR022                                                   *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome      | Request | Descrição                            *
*----------------------------------------------------------------------*
**13.05.20|JALEXANDRE |         | Gera  arquivo XML ANTAQ              *
*----------------------------------------------------------------------*
REPORT zcor022.

TABLES: t811f, zcot0013.
* =====================================================================
* Tipos
* =====================================================================
TYPES: BEGIN OF y_arquivo,
         record TYPE char255,
       END OF y_arquivo.

TYPES: BEGIN OF y_zcot0013.
         INCLUDE STRUCTURE zcot0013.
         TYPES: check_dupl TYPE char01,
       END OF y_zcot0013.

TYPES: BEGIN OF y_t0013,
         mesano   TYPE zcot0013-mesano,
         ciclo    TYPE zcot0013-ciclo,
         segmento TYPE zcot0013-segmento,
         cycle    TYPE t811f-cycle,
       END OF y_t0013.

TYPES: BEGIN OF y_item,
         cycle    TYPE t811f-cycle,
         seqnr    TYPE t811f-seqnr,
         name     TYPE t811s-name,
         element1 TYPE t811f-element1,
       END OF y_item.

TYPES: BEGIN OF y_collect,
         ciclo    TYPE zcot0013-ciclo,
         segmento TYPE zcot0013-segmento,
         perc     TYPE zcot0013-perc,
       END OF y_collect.

TYPES: BEGIN OF y_saida,
         mesano     TYPE zcot0013-mesano,
         kokrs      TYPE tka01-kokrs,
         ciclo      TYPE zcot0013-ciclo,
         segmento   TYPE zcot0013-segmento,
         txt        TYPE zcot0013-txt,
         receptor   TYPE zcot0013-receptor,
         perc       TYPE zcot0013-perc,
         status(40) TYPE c,

         perc_old   TYPE zcot0013-perc_old,

         usnam      TYPE zcot0013-usnam,
         zdt_atual  TYPE zcot0013-zdt_atual,
         zhr_atual  TYPE zcot0013-zhr_atual,
       END OF y_saida.

TYPES: BEGIN OF ty_saida,
         ciclo    TYPE zcot0013-ciclo,
         segmento TYPE zcot0013-segmento,
         des_seg  TYPE t811l-txt,
         receptor TYPE t811f-element1,
         perc     TYPE zcot0013-perc,
       END OF ty_saida.

TYPES: BEGIN OF y_seg,
         mesano    TYPE zcot0013-mesano,
         ciclo     TYPE zcot0013-ciclo,
         segmento  TYPE zcot0013-segmento,
         txt       TYPE zcot0013-txt,
         receptor  TYPE zcot0013-receptor,
         perc      TYPE zcot0013-perc,
         status    TYPE zcot0013-status,
         usnam     TYPE zcot0013-usnam,
         zdt_atual TYPE zcot0013-zdt_atual,
         zhr_atual TYPE zcot0013-zhr_atual,
         cycle     TYPE t811f-cycle,
       END OF y_seg.

TYPES: BEGIN OF y_t811k.
         INCLUDE STRUCTURE t811k.
         TYPES: setclass TYPE setleaf-setclass,
         setname2 TYPE setnamenew,
       END OF  y_t811k.

* =====================================================================
* Tabela Interna
* =====================================================================
DATA: t_t0013      TYPE TABLE OF y_t0013,
      t_seg        TYPE TABLE OF y_seg,
      t_zcot0013   TYPE TABLE OF y_zcot0013,"zcot0013,                  "zcot0013,
      wa_zcot0013  TYPE zcot0013,
      t_t811f      TYPE TABLE OF t811f,
      t_t811s      TYPE TABLE OF t811s,
      t_t811l      TYPE TABLE OF t811l,
      t_t811k      TYPE TABLE OF y_t811k,
      t_collect    TYPE TABLE OF y_collect,
      t_bdcdata    TYPE STANDARD TABLE OF bdcdata,
      t_saida      TYPE TABLE OF y_saida,
      t_upd        TYPE TABLE OF zcot0013,
      t_bdcmsgcoll TYPE TABLE OF bdcmsgcoll.

* =====================================================================
* Tabela Interna - Geração do XLS
* =====================================================================
DATA: it_zcot0013 TYPE TABLE OF zcot0013,
      it_saida    TYPE TABLE OF ty_saida.

* =====================================================================
* Tabela Interna
* =====================================================================
DATA: w_t0013      LIKE LINE OF t_t0013,
      w_zcot0013   LIKE LINE OF t_zcot0013,
      w_collect    LIKE LINE OF t_collect,
      w_t811f      LIKE LINE OF t_t811f,
      w_t811s      LIKE LINE OF t_t811s,
      w_t811l      LIKE LINE OF t_t811l,
      w_saida      LIKE LINE OF t_saida,
      w_bdcmsgcoll LIKE LINE OF t_bdcmsgcoll,
      w_options    TYPE ctu_params.

DATA: p_edit   TYPE c,
      ws_saida TYPE ty_saida.

* =====================================================================
* Declaração de Constantes
* =====================================================================
DATA: BEGIN OF w_icones,
        icon_erro(60)    TYPE c, "'E'. "Erro    - Percentual abaixo de 100 - Icone = X vermelho
        icon_sucesso(60) TYPE c, "'X'. "Sucesso - Percentual = 100         - Icone = Check verde
        icon_ksv2(60)    TYPE c,
        icon_proc(60)    TYPE c, " '' - Processado com sucesso
        icon_dele(60)    TYPE c,
        icon_defect(60)  TYPE c, "'D'. "Incompleto - Ordem não encontrada  - Bandeira Vermelha
        icon_change(60)  TYPE c,
        icon_save(60)    TYPE c,
        icon_locked(60)  TYPE c,
      END OF w_icones.

* =====================================================================
* Declaração de Ranges
* =====================================================================
DATA: r_selecao TYPE RANGE OF c,
      r_mesano  TYPE RANGE OF zcot0013-mesano,
      r_kokrs   TYPE RANGE OF zcot0013-kokrs.

* =====================================================================
* Field-Symbol
* =====================================================================
FIELD-SYMBOLS: <fs_fcat>  TYPE lvc_s_fcat.

* =====================================================================
* CLASS DEFINITION
* =====================================================================
CLASS       lcl_event_receiver DEFINITION FINAL.

  PUBLIC SECTION.

*---Method to handel toolbar
    METHODS :
      handle_toolbar
                  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.

*---Method to handel user_command
    METHODS :
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.


ENDCLASS .                    "LCL_EVENT_RECEIVER DEFINITION

* =====================================================================
* Instância
* =====================================================================
DATA: v_grid           TYPE REF TO cl_gui_alv_grid,         "#EC NEEDED
      v_event_receiver TYPE REF TO lcl_event_receiver,      "#EC NEEDED
      v_docking        TYPE REF TO cl_gui_docking_container,
      v_splitter       TYPE REF TO cl_gui_splitter_container,
      v_container_1    TYPE REF TO cl_gui_container,
      v_container_2    TYPE REF TO cl_gui_container.


TYPES: BEGIN OF ty_ucomm,
         ucomm TYPE  sy-ucomm,
       END OF ty_ucomm.

DATA: it_ucomm TYPE TABLE OF ty_ucomm.

DATA: p_ordem    TYPE i.
* =====================================================================
* CLASS IMPLEMENTATION
* =====================================================================
CLASS lcl_event_receiver IMPLEMENTATION.

*-----Logic to handle the ToolBar
  METHOD handle_toolbar.
    PERFORM zf_elimina_botoes_header  USING e_object.
    PERFORM zf_adiciona_botoes_header USING e_object.
  ENDMETHOD.

  METHOD handle_user_command.

    DATA: t_fcat TYPE lvc_t_fcat.

    CASE e_ucomm.
      WHEN 'DELE'.
        PERFORM zf_eliminar_registro.
      WHEN 'KSV2'.
        PERFORM zf_executar_ksv2.
      WHEN 'EDIT'.

        IF v_grid IS NOT INITIAL.

          p_edit = abap_true.

          REFRESH: t_fcat[].
          CALL METHOD v_grid->get_frontend_fieldcatalog
            IMPORTING
              et_fieldcatalog = t_fcat[].

          LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
            IF <fs_fcat>-fieldname = 'PERC'.
              <fs_fcat>-edit      = abap_true.
            ENDIF.
          ENDLOOP.

          CALL METHOD v_grid->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = t_fcat[].

          CALL METHOD v_grid->refresh_table_display.
        ENDIF.

      WHEN 'SAVE'.
        PERFORM zf_update_zcot0013.
      WHEN 'ENTER'.

    ENDCASE.

    CALL METHOD v_grid->refresh_table_display.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
* =====================================================================
* Parâmetros de Seleção
* =====================================================================
SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS: p_mesano TYPE spmon OBLIGATORY,
            p_kokrs  TYPE tka01-kokrs OBLIGATORY.
SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 11(20) text-002 FOR FIELD r_arq.
SELECTION-SCREEN POSITION 10.
PARAMETERS: r_arq  RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND sel.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_file TYPE localfile.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 40(35) text-003 FOR FIELD r_ksv2.
SELECTION-SCREEN POSITION 10.
PARAMETERS: r_ksv2 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_layout      TYPE disvariant-variant MODIF ID t1 DEFAULT '/STD' NO-DISPLAY.

SELECTION-SCREEN: END OF BLOCK  bl1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-028.
PARAMETERS: r_tod RADIOBUTTON GROUP 1 MODIF ID ksv DEFAULT 'X'. "Todos
PARAMETERS: r_apr RADIOBUTTON GROUP 1 MODIF ID ksv. "A processar
PARAMETERS: r_prc RADIOBUTTON GROUP 1 MODIF ID ksv. "Processado
PARAMETERS: r_err RADIOBUTTON GROUP 1 MODIF ID ksv. "Com erro
PARAMETERS: r_fts RADIOBUTTON GROUP 1 MODIF ID ksv. "Falta segmentos da planilha
PARAMETERS: r_grp RADIOBUTTON GROUP 1 MODIF ID ksv. "Gerar planilha excel com segmento cadastrado.
PARAMETERS p_ciclo TYPE zcot0013-ciclo MODIF ID ksv. "xls.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

*  PERFORM zf_tratar_campos.

*  IF r_grp IS INITIAL.
*    PERFORM zf_consistir_tela_selecao.
*  ENDIF.
  PERFORM zf_monta_tela.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM zf_definir_caminho_arquivo.


* =====================================================================
*START-OF-SELECTION.
* =====================================================================
START-OF-SELECTION.

  PERFORM zf_limpar_globais    .

  IF r_arq IS NOT INITIAL.

    PERFORM zf_importar_arquivo.

  ELSEIF r_ksv2 IS NOT INITIAL.

    IF r_grp IS INITIAL.

      PERFORM zf_consistir_tela_selecao.
      PERFORM zf_processamento_ksv2.

    ELSE.

      PERFORM sf_selec_segmento.

      IF it_saida[] IS NOT INITIAL.
        PERFORM gera_arquivo.
      ENDIF.

    ENDIF.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  ZF_DEFINIR_CAMINHO_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_definir_caminho_arquivo .

  DATA: t_filename TYPE filetable,
        w_filename LIKE LINE OF t_filename.

  DATA: l_subrc TYPE sy-subrc.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Buscar arquivo para importação'
      default_extension       = 'XLSX'
*     DEFAULT_FILENAME        = ''
*     FILE_FILTER             =
*     INITIAL_DIRECTORY       = 'C:\Documents and Settings\kiran\Desktop\'
*     MULTISELECTION          =
*     WITH_ENCODING           =
    CHANGING
      file_table              = t_filename
      rc                      = l_subrc
*     USER_ACTION             =
*     FILE_ENCODING           =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.

  ELSE.

    LOOP AT t_filename INTO w_filename.
      p_file = w_filename-filename.
      EXIT.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_LIMPAR_GLOBAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_limpar_globais .

  REFRESH: t_zcot0013, t_collect,
           t_t811f, t_t811s, t_t811l, t_seg.

  CLEAR:   w_zcot0013, w_collect,
           t_t811f, t_t811s, t_t811l.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_LER_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_ler_arquivo .

  DATA: w_seg LIKE LINE OF t_seg.

  IF p_file IS NOT INITIAL.

    TYPES: BEGIN OF ty_table,
             ciclo    TYPE string,
             segmento TYPE string,
             txt      TYPE string,
             receptor TYPE string,
             perc     TYPE string,
           END OF ty_table.

    DATA: t_intern  TYPE STANDARD TABLE OF alsmex_tabline,
          t_arquivo TYPE STANDARD TABLE OF ty_table, " WITH HEADER LINE.
          t_delete  TYPE TABLE OF zcot0013,

          w_arquivo LIKE LINE OF t_arquivo.

    FIELD-SYMBOLS: <fs_intern> LIKE LINE OF t_intern,
                   <fs_aux>.

    DATA: v_index TYPE i.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_file
        i_begin_col             = '1'
        i_begin_row             = '1'
        i_end_col               = '256'
        i_end_row               = '65536'
      TABLES
        intern                  = t_intern
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      MESSAGE i000(z_co) WITH 'Erro na leitura do arquivo.'(010).
    ENDIF.

  ELSE.
    MESSAGE s000(z_co) WITH 'Caminho do arquivo não informado.'(007) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DELETE t_intern WHERE row EQ '0001'.

  SORT t_intern BY row col.
  LOOP AT t_intern ASSIGNING <fs_intern>.
    MOVE <fs_intern>-col TO v_index.
    ASSIGN COMPONENT v_index OF STRUCTURE w_arquivo TO <fs_aux>.
    MOVE <fs_intern>-value TO <fs_aux>.
    AT END OF row.
      APPEND w_arquivo TO t_arquivo.
      CLEAR w_arquivo.
    ENDAT.
  ENDLOOP.

  LOOP AT t_arquivo INTO w_arquivo.

    w_zcot0013-mesano    = p_mesano+4(2) && p_mesano(4).  "Mês Fechamento (Parâmetro)
    w_zcot0013-kokrs     = p_kokrs.  "Mês Fechamento (Parâmetro)
    w_zcot0013-ciclo     = w_arquivo-ciclo.  "Ciclo (Planilha Excel coluna A)
    w_zcot0013-segmento  = w_arquivo-segmento.  "Segmento (Planilha Excel coluna B)
    w_zcot0013-txt       = w_arquivo-txt.  "Segmento (Planilha Excel coluna B)
    w_zcot0013-receptor  = w_arquivo-receptor.  "Receptor (Planilha Excel coluna D)

    REPLACE ALL OCCURRENCES OF '-' IN w_arquivo-perc WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN w_arquivo-perc WITH '.'.
    CONDENSE w_arquivo-perc.

    w_zcot0013-perc  = w_arquivo-perc. "Quota/percent. (Planilha Excel coluna E)

    w_zcot0013-usnam     = sy-uname.  "usuário de login
    w_zcot0013-zdt_atual = sy-datum. "data do sistema
    w_zcot0013-zhr_atual = sy-uzeit. "hora do sistema

    w_collect-ciclo     = w_zcot0013-ciclo.
    w_collect-segmento  = w_zcot0013-segmento.
    w_collect-perc      = w_zcot0013-perc.

* Efetua somatória
    COLLECT w_collect INTO t_collect.

* Atualiza tabela Z
    APPEND w_zcot0013 TO t_zcot0013.

    MOVE-CORRESPONDING w_zcot0013 TO w_seg.

    CONCATENATE p_kokrs w_seg-ciclo INTO w_seg-cycle.
    APPEND w_seg TO t_seg.

    CLEAR: w_collect, w_zcot0013, w_arquivo, w_seg.
  ENDLOOP.

  IF t_zcot0013[] IS NOT INITIAL.

    REFRESH: r_mesano, r_kokrs .
    r_mesano = VALUE #( ( sign = 'I' option = 'EQ' low = p_mesano )
                        ( sign = 'I' option = 'EQ' low = p_mesano+4(2) && p_mesano(4) ) ).

    r_kokrs = VALUE #( ( sign = 'I' option = 'EQ' low = '' )
                       ( sign = 'I' option = 'EQ' low = p_kokrs ) ).

* Busca Registros carregados para o MêsAno, Ciclo e Segmento
    SELECT * FROM zcot0013
      INTO TABLE t_delete
      FOR ALL ENTRIES IN t_zcot0013
      WHERE mesano   IN r_mesano   "= t_zcot0013-mesano
        AND kokrs    IN r_kokrs
        AND ciclo    = t_zcot0013-ciclo
        AND segmento = t_zcot0013-segmento.

    DELETE zcot0013 FROM TABLE t_delete.
    COMMIT WORK.

    REFRESH:  t_delete.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_montar_saida .

  DATA: l_aufnr          TYPE aufk-aufnr,
        l_objnr          TYPE jsto-objnr,
        v_receptor       TYPE zcot0013-receptor,
        v_segmneto       TYPE zcot0013-segmento,
        r_receptor       TYPE RANGE OF zcot0013-receptor,
        l_error_occurred TYPE c.

  CLEAR: v_receptor, v_segmneto.
  FREE: t_saida, r_receptor.

**Inicio BUG SOLTO 67614 =========================// Anderson Oenning - 17/02/2022.
  "Remover ordem repetida e com valor 0.
  IF t_zcot0013 IS NOT INITIAL .
    SORT t_zcot0013 BY segmento receptor.

    LOOP AT t_zcot0013 INTO DATA(ls_zcot0013).
      IF ls_zcot0013-receptor EQ v_receptor AND ls_zcot0013-segmento EQ v_segmneto.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_zcot0013-receptor ) TO r_receptor.
        ls_zcot0013-check_dupl = abap_true.
      ENDIF.

*      v_receptor = ls_zcot0013-receptor.
*      v_segmneto = ls_zcot0013-segmento.
    ENDLOOP.

*    IF r_receptor IS NOT INITIAL.
      sort t_zcot0013 by check_dupl.
*      DELETE t_zcot0013 WHERE receptor IN r_receptor AND perc EQ 0.
      DELETE t_zcot0013 WHERE CHECK_DUPL EQ abap_true.
*    ENDIF.

    IF t_zcot0013 IS NOT INITIAL.
      SORT t_zcot0013 BY kokrs.
    ENDIF.
  ENDIF.
**Fim BUG SOLTO 67614 =========================// Anderson Oenning - 17/02/2022.

*  sort t_zcot0013 by CICLO TXT.

  LOOP AT t_zcot0013 INTO w_zcot0013.
     "Verifica o status da ordem
      CLEAR: l_aufnr, l_error_occurred, w_saida.

    w_saida-mesano   = w_zcot0013-mesano.
    w_saida-kokrs    = w_zcot0013-kokrs.
    w_saida-ciclo    = w_zcot0013-ciclo.
    w_saida-segmento = w_zcot0013-segmento.
    w_saida-txt      = w_zcot0013-txt.
    w_saida-receptor = w_zcot0013-receptor.
    w_saida-perc     = w_zcot0013-perc.
    w_saida-status   = w_zcot0013-status.

    IF w_saida-status = 'X'.



      l_aufnr = w_saida-receptor.
      UNPACK l_aufnr TO l_aufnr.

      "Cabeçalho do Ordem
      SELECT SINGLE * FROM aufkv
        INTO @DATA(w_aufkv)
        WHERE aufnr =  @l_aufnr.

      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'K_ORDER_CHECK'
          EXPORTING
            aufnr          = w_aufkv-aufnr "Nro. Ordem
            e_aufkv        = w_aufkv       "select single da ordem
            test_bukrs     = ''            "em branco
            test_kokrs     = w_saida-kokrs "centro de custo
            test_vrgng     = 'RKIV'
            actvt          = '02'          "03 não valida / 02 valida - passar
          IMPORTING
            error_occurred = l_error_occurred
          EXCEPTIONS
            OTHERS         = 1.

        IF sy-subrc IS NOT INITIAL.
          w_saida-status = 'B'.
        ENDIF.

      ENDIF.

    ENDIF.

    CASE w_saida-status.
      WHEN 'E'. "'E'. "Erro    - Percentual abaixo de 100 - Icone = X vermelho
        w_saida-status          = w_icones-icon_erro.
      WHEN 'P'."Processado  com sucesso
        w_saida-status          = w_icones-icon_proc.
      WHEN 'D'."'D'. "Incompleto - Ordem não encontrada  - Bandeira Vermelha
        w_saida-status          = w_icones-icon_defect.
      WHEN 'B'."Bloqueado
        w_saida-status          = w_icones-icon_locked.
      WHEN OTHERS.
        w_saida-status          = w_icones-icon_sucesso.
    ENDCASE.


    w_saida-usnam     = w_zcot0013-usnam.
    w_saida-zdt_atual = w_zcot0013-zdt_atual.
    w_saida-zhr_atual = w_zcot0013-zhr_atual.

    w_saida-perc_old     = w_zcot0013-perc_old.

    APPEND w_saida  TO t_saida.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_selecionar_dados .

  REFRESH: t_zcot0013.

  PERFORM zf_icon_create USING icon_checked        text-024 CHANGING w_icones-icon_sucesso.
  PERFORM zf_icon_create USING icon_incomplete     text-005 CHANGING w_icones-icon_erro.
  PERFORM zf_icon_create USING icon_execute_object text-014 CHANGING w_icones-icon_ksv2.
  PERFORM zf_icon_create USING icon_complete      text-015 CHANGING w_icones-icon_proc.
  PERFORM zf_icon_create USING icon_delete         text-016 CHANGING w_icones-icon_dele.
  PERFORM zf_icon_create USING icon_system_save    text-034 CHANGING w_icones-icon_save.
  PERFORM zf_icon_create USING icon_defect         text-027 CHANGING w_icones-icon_defect.
  PERFORM zf_icon_create USING icon_change         text-032 CHANGING w_icones-icon_change.
  PERFORM zf_icon_create USING icon_locked         text-032 CHANGING w_icones-icon_locked.

  REFRESH: r_mesano, r_kokrs .
  r_mesano = VALUE #( ( sign = 'I' option = 'EQ' low = p_mesano )
                      ( sign = 'I' option = 'EQ' low = p_mesano+4(2) && p_mesano(4) ) ).

  r_kokrs = VALUE #( ( sign = 'I' option = 'EQ' low = '' )
                     ( sign = 'I' option = 'EQ' low = p_kokrs ) ).

  "Check seleção filtro pesquisa.
  FREE: r_selecao.
  CASE abap_true.
    WHEN r_tod. "Todos

    WHEN r_apr. "A processar - Percentual = 100 - Icone = Check verde
      r_selecao = VALUE #( ( sign = 'I' option = 'EQ' low = 'X' ) ).

    WHEN r_prc. "Processado - Processado com Sucesso - Bandeira Quadriculada
      r_selecao = VALUE #( ( sign = 'I' option = 'EQ' low = 'P' ) ).

    WHEN r_err. "Com erro - Percentual abaixo de 100 - Icone = X vermelho
      r_selecao = VALUE #( ( sign = 'I' option = 'EQ' low = 'E' )
                           ( sign = 'I' option = 'EQ' low = 'B' ) ).

    WHEN r_fts. "Incompleto - Ordem não encontrada  - Bandeira Vermelha
      r_selecao = VALUE #( ( sign = 'I' option = 'EQ' low = 'D' ) ).
    WHEN OTHERS.
  ENDCASE.

* Importação Ciclo de Distribuição para KSV2
  IF r_tod IS INITIAL.

* Busca Registro selecionado
    SELECT * FROM zcot0013
      INTO TABLE @DATA(t_sel_0013)
      WHERE mesano IN @r_mesano "= @p_mesano
       AND kokrs    IN @r_kokrs
       AND status IN @r_selecao.

* Busca todos os itens do registros selecionado
    IF t_sel_0013[] IS NOT INITIAL.

      SELECT * FROM zcot0013
        INTO TABLE t_zcot0013
        FOR ALL ENTRIES IN t_sel_0013
      WHERE mesano  IN r_mesano "= @p_mesano
       AND kokrs    IN r_kokrs
         AND ciclo    = t_sel_0013-ciclo
         AND segmento = t_sel_0013-segmento.

    ENDIF.

  ELSE.

    SELECT * FROM zcot0013
      INTO TABLE t_zcot0013
      WHERE mesano IN r_mesano "= @p_mesano
       AND kokrs    IN r_kokrs.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_IMPORTAR_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_importar_arquivo .
  PERFORM zf_ler_arquivo.
  PERFORM zf_dados_complementares.
  PERFORM zf_validar_dados.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSAMENTO_KSV2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_processamento_ksv2 .
  PERFORM zf_selecionar_dados.
  PERFORM zf_montar_saida.
  PERFORM zf_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ICON_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0672   text
*      -->P_TEXT_014  text
*      <--P_W_ICONES_ICON_COMPLETE  text
*----------------------------------------------------------------------*
FORM zf_icon_create    USING  p_nome_icone
                              p_texto
                              p_icone.

*  CALL FUNCTION 'ICON_CREATE'
*    EXPORTING
*      name   = p_nome_icone
*      info   = p_texto
*    IMPORTING
*      result = p_icone.

  p_icone = p_nome_icone.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv .

  IF t_saida[] IS NOT INITIAL.
    CALL SCREEN 9000.
    LEAVE LIST-PROCESSING.
  ELSE.
    MESSAGE i398(00) DISPLAY LIKE 'E' WITH 'Nenhum registro encontrado.'(006).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXECUTAR_KSV2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_executar_ksv2.

  DATA: t_saida_aux TYPE TABLE OF y_saida,
        w_saida_aux LIKE LINE OF t_saida.

  DATA: t_saida2 TYPE TABLE OF y_saida,
        w_saida2 LIKE LINE OF t_saida2.

  DATA: l_auth_check      TYPE REF TO cx_root,
        l_auth_check_text TYPE string,
        l_tabix           TYPE sy-tabix,
        l_tabix2          TYPE sy-tabix,
        l_percent         TYPE c LENGTH 10.

  CLEAR w_options.
  w_options-dismode  = 'N'.
  w_options-updmode  = 'A'.
  w_options-cattmode = ' '.
  w_options-defsize  = 'X'.
  w_options-racommit = 'X'.
  w_options-nobinpt  = ' '.
  w_options-nobiend  = ' '.

  SORT t_zcot0013 BY ciclo segmento receptor.
  SORT t_saida    BY ciclo segmento receptor.

  REFRESH:  t_saida2[].
  t_saida2[] =  t_saida[].

*  t_saida_aux[] = t_saida[].
*  DELETE ADJACENT DUPLICATES FROM t_saida_aux COMPARING ciclo segmento.

* Define àrea de centro de custo
  SET PARAMETER ID 'CAC' FIELD p_kokrs.

***********************************************

  " Copy original table
**  DATA(t_filter) = t_saida[].
**
**  " Get excluded rows
**  v_grid->get_filtered_entries(
**    IMPORTING
**      et_filtered_entries = DATA(t_index)
**  ).
**
**  IF t_index[] IS NOT INITIAL.
**
**    " Reverse order to keep correct indizes; thnx futu
**    SORT t_index DESCENDING.
**
**    " Remove excluded rows from buffer
**    LOOP AT t_index ASSIGNING FIELD-SYMBOL(<index>).
**      DELETE t_filter INDEX <index>.
**    ENDLOOP.
**
**    REFRESH: t_saida_aux.
**    t_saida_aux[] = t_filter[].
**
**  ENDIF.

***********************************************
  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_saida INTO w_saida_aux   INDEX w_row_no-row_id.

    READ TABLE t_saida TRANSPORTING NO FIELDS WITH KEY ciclo    = w_saida_aux-ciclo
                                                segmento = w_saida_aux-segmento
                                                status   = w_icones-icon_proc. "'P'.

    CHECK sy-subrc IS NOT INITIAL.

    READ TABLE t_saida TRANSPORTING NO FIELDS WITH KEY ciclo    = w_saida_aux-ciclo
                                                segmento = w_saida_aux-segmento
                                                status   = w_icones-icon_erro."'E'.

    CHECK sy-subrc IS NOT INITIAL.

    READ TABLE t_saida TRANSPORTING NO FIELDS WITH KEY ciclo    = w_saida_aux-ciclo
                                                segmento = w_saida_aux-segmento
                                                status   = w_icones-icon_erro."'E'.

    CHECK sy-subrc IS NOT INITIAL.

    "'D'. "Incompleto - Ordem não encontrada  - Bandeira Vermelha
    READ TABLE t_saida TRANSPORTING NO FIELDS WITH KEY ciclo    = w_saida_aux-ciclo
                                             segmento = w_saida_aux-segmento
                                             status   = w_icones-icon_defect. "'D'.

    CHECK sy-subrc IS NOT INITIAL.

    "'B'. "Bloqueado    - Erro na Ordem
    READ TABLE t_saida TRANSPORTING NO FIELDS WITH KEY ciclo    = w_saida_aux-ciclo
                                             segmento = w_saida_aux-segmento
                                             status   = w_icones-icon_locked."'B'.

    CHECK sy-subrc IS NOT INITIAL.

    APPEND w_saida_aux TO t_saida_aux.

  ENDLOOP.

  SORT t_saida_aux BY ciclo segmento.
  DELETE ADJACENT DUPLICATES FROM t_saida_aux COMPARING ciclo segmento.

  LOOP AT t_saida_aux INTO w_saida_aux WHERE status = w_icones-icon_sucesso.

    REFRESH: t_bdcdata, t_bdcmsgcoll, t_upd.

    "'E'. "Erro    - Percentual abaixo de 100 - Icone = X vermelho
    READ TABLE t_saida INTO w_saida WITH KEY ciclo    = w_saida_aux-ciclo
                                             segmento = w_saida_aux-segmento
                                             status   = w_icones-icon_erro."'E'.

    CHECK sy-subrc IS NOT INITIAL.

    REFRESH: t_bdcdata, t_bdcmsgcoll, t_upd.
    "'D'. "Incompleto - Ordem não encontrada  - Bandeira Vermelha
    READ TABLE t_saida INTO w_saida WITH KEY ciclo    = w_saida_aux-ciclo
                                             segmento = w_saida_aux-segmento
                                             status   = w_icones-icon_defect. "'D'.

    CHECK sy-subrc IS NOT INITIAL.

    REFRESH: t_bdcdata, t_bdcmsgcoll, t_upd.
    "'B'. "Bloqueado    - Erro na Ordem
    READ TABLE t_saida INTO w_saida WITH KEY ciclo    = w_saida_aux-ciclo
                                             segmento = w_saida_aux-segmento
                                             status   = w_icones-icon_locked."'B'.

    CHECK sy-subrc IS NOT INITIAL.
*----------------------------------------------------------------------*
* ZERA ORDENS
*----------------------------------------------------------------------*

    PERFORM zf_dynpro USING 'SAPMKAL1'  '0103'.
    PERFORM zf_field  USING 'BDC_CURSOR'  'RKAL1-KSCYC'.
    PERFORM zf_field  USING 'BDC_OKCODE'  '=ENA'.
    PERFORM zf_field  USING 'RKAL1-KSCYC' w_saida_aux-ciclo.

    PERFORM zf_dynpro USING 'SAPMKAL1'  '0201'.
    PERFORM zf_field  USING 'BDC_CURSOR'  'RKAL1-CTXT'.
    PERFORM zf_field  USING 'BDC_OKCODE'  '=SQOV'.

    PERFORM zf_dynpro USING 'SAPLKGAL'  '0100'.
    PERFORM zf_field  USING 'BDC_CURSOR'  'KGALS-NAME(01)'.
    PERFORM zf_field  USING 'BDC_OKCODE'  '=SUCH'.

    PERFORM zf_dynpro USING 'SAPLKGAL'  '0120'.
    PERFORM zf_field  USING 'BDC_CURSOR'  'SEARCH_STRUC-NAME'.
    PERFORM zf_field  USING 'BDC_OKCODE'  '=ENTE'.
    PERFORM zf_field  USING 'TEXT_T02_ON' 'X'.
    PERFORM zf_field  USING 'SEARCH_STRUC-NAME' w_saida_aux-segmento.

    PERFORM zf_dynpro USING 'SAPLKGAL'  '0100'.
    PERFORM zf_field  USING 'BDC_CURSOR'  'KGALS-NAME(01)'.
    PERFORM zf_field  USING 'BDC_OKCODE'  '=SELE'.

    PERFORM zf_dynpro USING 'SAPMKAL1'  '0300'.
    PERFORM zf_field  USING 'BDC_CURSOR'  'KGALS-NAME'.
    PERFORM zf_field  USING 'BDC_OKCODE'  '=RECE'.

    READ TABLE t_saida2 INTO w_saida2 WITH KEY ciclo     = w_saida_aux-ciclo
                                             segmento  = w_saida_aux-segmento
                                                                      BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      l_tabix2 = sy-tabix.

      DO.

        "zera apenas ordens com valores na T811f
        IF  w_saida2-perc_old IS NOT INITIAL.

          PERFORM zf_dynpro USING 'SAPMKAL1'     '0300'.
          PERFORM zf_field  USING 'BDC_CURSOR'   'KGALS-NAME'.
          PERFORM zf_field  USING 'BDC_OKCODE'   '=SEARCH_OBJECT'.

          PERFORM zf_dynpro USING 'SAPLSPO4'  '0300'.
          PERFORM zf_field  USING 'BDC_CURSOR'  'SVALD-VALUE(01)'.
          PERFORM zf_field  USING 'BDC_OKCODE'  '=FURT'.
          PERFORM zf_field  USING 'SVALD-VALUE(01)' w_saida2-receptor.

          PERFORM zf_dynpro USING 'SAPMKAL1'  '0300'.
          PERFORM zf_field  USING 'BDC_OKCODE'  '/00'.
          PERFORM zf_field  USING 'BDC_CURSOR'  'KGALF-PERCENT(01)'.

          l_percent = '0.00'. ""W_SAIDA-PERC.
          CONDENSE l_percent.
          REPLACE '.' IN l_percent WITH ','.

          PERFORM zf_field  USING 'KGALF-PERCENT(01)' l_percent.

        ENDIF.

        ADD 1 TO l_tabix2.
        READ TABLE t_saida2 INTO w_saida2 INDEX l_tabix2.

        IF sy-subrc IS NOT INITIAL
              OR w_saida_aux-ciclo <> w_saida2-ciclo
              OR w_saida_aux-segmento <> w_saida2-segmento.
          EXIT.
        ENDIF.
      ENDDO.

    ENDIF.


*----------------------------------------------------------------------*
* Preenche novo valor das ordens
*----------------------------------------------------------------------*
    READ TABLE t_saida INTO w_saida WITH KEY ciclo     = w_saida_aux-ciclo
                                             segmento  = w_saida_aux-segmento
                                                                      BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      l_tabix = sy-tabix.

      DO.

        "considera apenas ordens com valores importados no excel
        IF  w_saida-perc IS NOT INITIAL.

          PERFORM zf_dynpro USING 'SAPMKAL1'     '0300'.
          PERFORM zf_field  USING 'BDC_CURSOR'   'KGALS-NAME'.
          PERFORM zf_field  USING 'BDC_OKCODE'   '=SEARCH_OBJECT'.

          PERFORM zf_dynpro USING 'SAPLSPO4'  '0300'.
          PERFORM zf_field  USING 'BDC_CURSOR'  'SVALD-VALUE(01)'.
          PERFORM zf_field  USING 'BDC_OKCODE'  '=FURT'.
          PERFORM zf_field  USING 'SVALD-VALUE(01)' w_saida-receptor.

          PERFORM zf_dynpro USING 'SAPMKAL1'  '0300'.
          PERFORM zf_field  USING 'BDC_OKCODE'  '/00'.
          PERFORM zf_field  USING 'BDC_CURSOR'  'KGALF-PERCENT(01)'.

          l_percent = w_saida-perc.
          CONDENSE l_percent.
          REPLACE '.' IN l_percent WITH ','.

          PERFORM zf_field  USING 'KGALF-PERCENT(01)' l_percent.

        ENDIF.

        READ TABLE t_zcot0013 INTO w_zcot0013 WITH KEY ciclo = w_saida-ciclo
                                                    segmento = w_saida-segmento
                                                    receptor = w_saida-receptor
                                                                          BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          APPEND w_zcot0013 TO t_upd.
        ENDIF.

        ADD 1 TO l_tabix.
        READ TABLE t_saida INTO w_saida INDEX l_tabix.

        IF sy-subrc IS NOT INITIAL
              OR w_saida_aux-ciclo <> w_saida-ciclo
              OR w_saida_aux-segmento <> w_saida-segmento.

          PERFORM zf_dynpro USING 'SAPMKAL1'          '0300'.
          PERFORM zf_field USING  'BDC_OKCODE'        '=SAVE'.

          PERFORM zf_dynpro USING 'SAPMKAL1'          '0300'.
          PERFORM zf_field USING 'BDC_OKCODE'        '/EEND'.

          TRY.

              CALL TRANSACTION 'KSV2' WITH AUTHORITY-CHECK USING t_bdcdata
                             OPTIONS FROM w_options
                             MESSAGES INTO t_bdcmsgcoll.

            CATCH cx_sy_authorization_error INTO l_auth_check.
*     Authorization missing for user when executing transaction
              l_auth_check_text = l_auth_check->get_text( ).
              sy-subrc = 99.

          ENDTRY.

          EXIT.

        ENDIF.

      ENDDO.

* verifica a primeira mensagem do segmento
      PERFORM zf_mostra_mensagem.

    ENDIF.

    CLEAR: w_saida_aux, w_saida.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_  PERFORM ZF_FIELD  USING 'BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1117   text
*      -->P_1118   text
*----------------------------------------------------------------------*
FORM zf_dynpro   USING p_prog
                         p_scr.

  DATA: w_bdcdata LIKE LINE OF t_bdcdata.

  w_bdcdata-program  = p_prog.
  w_bdcdata-dynpro   = p_scr.
  w_bdcdata-dynbegin = abap_true.

  APPEND w_bdcdata TO t_bdcdata.
  CLEAR w_bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_  PERFORM ZF_FIELD  USING 'BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1122   text
*      -->P_1123   text
*----------------------------------------------------------------------*
FORM zf_field USING p_fnam
                         p_fval.

  DATA: w_bdcdata LIKE LINE OF t_bdcdata.

  w_bdcdata-fnam   = p_fnam.
  w_bdcdata-fval   = p_fval.

  APPEND w_bdcdata TO t_bdcdata.
  CLEAR w_bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_status_9000 OUTPUT.

  DATA: w_ucomm TYPE sy-ucomm.
  FREE: it_ucomm.

*  IF p_edit IS INITIAL.
  w_ucomm = 'SAVE'.
  APPEND w_ucomm TO it_ucomm.
  CLEAR w_ucomm.

  SET PF-STATUS 'ZGMM_STATUS_9000' EXCLUDING it_ucomm..
  SET TITLEBAR  'ZUMM_TITULO'.

  IF p_edit IS INITIAL.
    PERFORM zf_split_screen.
  ENDIF.
  PERFORM zf_preparar_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_alv .

  DATA: t_fcat TYPE lvc_t_fcat,
        t_sort TYPE lvc_t_sort.

  DATA: w_variant TYPE disvariant,
        w_layout  TYPE lvc_s_layo.

  w_layout-cwidth_opt = 'X'.
  w_layout-zebra      = 'X'.
  w_variant-report    = sy-repid.
  w_variant-variant   = p_layout.


  PERFORM zf_montar_fieldcat     CHANGING t_saida t_fcat.
  PERFORM zf_ajuste_descr_campos CHANGING t_fcat.
  PERFORM zf_ordernar_subtotal   CHANGING t_sort.

  IF p_edit IS NOT INITIAL.
    LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      IF <fs_fcat>-fieldname EQ 'PERC'.
        <fs_fcat>-edit      = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF v_grid IS INITIAL.

    CREATE OBJECT v_grid
      EXPORTING
        i_parent          = v_container_2 "V_CONTAINER "CL_GUI_CONTAINER=>DEFAULT_SCREEN
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT v_event_receiver.
    SET HANDLER v_event_receiver->handle_toolbar       FOR v_grid.
    SET HANDLER v_event_receiver->handle_user_command  FOR v_grid.

    w_layout-sel_mode = 'A'.

    CALL METHOD v_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = w_variant
        i_save                        = 'A'
        is_layout                     = w_layout
      CHANGING
        it_fieldcatalog               = t_fcat
        it_outtab                     = t_saida
        it_sort                       = t_sort
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   RAISE EVENT TOOLBAR TO SHOW THE MODIFIED TOOLBAR
    CALL METHOD v_grid->set_toolbar_interactive.

  ELSE.
    CALL METHOD v_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fcat[].

    CALL METHOD v_grid->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_montar_fieldcat  CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = l_salv_table
        CHANGING
          t_table      = <f_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

* Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_elimina_botoes_header USING e_object TYPE REF TO cl_alv_event_toolbar_set.

*    elimina itens desnecessarios da barra do container
*  REFRESH: E_OBJECT->MT_TOOLBAR.

  DELETE e_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                 OR function = '&LOCAL&INSERT_ROW'
                                 OR function = '&LOCAL&DELETE_ROW'
                                 OR function = '&LOCAL&COPY_ROW'
                                 OR function = '&LOCAL&CUT'
                                 OR function = '&LOCAL&COPY'
                                 OR function = '&LOCAL&PASTE'
                                 OR function = '&REFRESH'
                                 OR function = '&CHECK'
                                 OR function = '&GRAPH'
                                 OR function = '&INFO'
                                 OR function = '&LOCAL&UNDO'
                                 OR function = '&MB_VIEW'
*                                   OR function = '&MB_VARIANT'
*                                 OR FUNCTION = '&MB_EXPORT'
                                 OR function = '&PRINT_BACK'
                                 OR function = '&MB_SUM'
                                 OR function = '&MB_SUBTOT'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_adiciona_botoes_header  USING e_object TYPE REF TO cl_alv_event_toolbar_set.

* Add Button
  DATA: w_toolbar  TYPE stb_button.

* inclui novo item na barra do container
  CLEAR w_toolbar.
  MOVE 3 TO w_toolbar-butn_type.
  APPEND w_toolbar TO e_object->mt_toolbar.

* inclui novo item na barra do container
*  PERFORM zf_icon_create USING 'ICON_TABLE_SETTINGS' text-006 CHANGING w_icones-icon_table_settings.

  CLEAR w_toolbar.
  MOVE 'KSV2'                     TO w_toolbar-function.
  MOVE w_icones-icon_ksv2         TO w_toolbar-icon.
  MOVE '0 '                       TO w_toolbar-butn_type.
  MOVE 'Executar KSV2'(008)       TO w_toolbar-quickinfo.
  MOVE 'Executar KSV2'(008)       TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

* INCLUI NOVO ITEM NA BARRA DO CONTAINER
  CLEAR w_toolbar.
  MOVE 3 TO w_toolbar-butn_type.
  APPEND w_toolbar TO e_object->mt_toolbar.

* inclui novo item na barra do container
*  PERFORM zf_icon_create USING 'ICON_TABLE_SETTINGS' text-006 CHANGING w_icones-icon_table_settings.

  CLEAR w_toolbar.
  MOVE 'EDIT'                     TO w_toolbar-function.
  MOVE w_icones-icon_change       TO w_toolbar-icon.
  MOVE '0 '                       TO w_toolbar-butn_type.
  MOVE 'Editar Percentual'(032)   TO w_toolbar-quickinfo.
  MOVE 'Editar Percentual'(032)   TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

  CLEAR w_toolbar.
  MOVE 'DELE'                     TO w_toolbar-function.
  MOVE w_icones-icon_dele         TO w_toolbar-icon.
  MOVE '0 '                       TO w_toolbar-butn_type.
  MOVE 'Eliminar Registros não Proc'(016)   TO w_toolbar-quickinfo.
  MOVE 'Eliminar Registros não Processados'(016)   TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

*  IF p_edit IS NOT INITIAL.
  CLEAR w_toolbar.
  MOVE 'SAVE'                     TO w_toolbar-function.
  MOVE w_icones-icon_save         TO w_toolbar-icon.
  MOVE '0 '                       TO w_toolbar-butn_type.

  IF p_edit IS  INITIAL.
    MOVE abap_true                     TO  w_toolbar-disabled.
  ENDIF.

  MOVE 'Salvar percentual'(034)   TO w_toolbar-quickinfo.
  MOVE 'Salvar percentual'(034)   TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command_9000 INPUT.

  CASE sy-ucomm.

    WHEN 'SAVE'.
*      CALL METHOD v_grid->refresh_table_display.
*      PERFORM zf_update_zcot0013.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTE_DESCR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_ajuste_descr_campos  CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING <fs_fcat>.
    PERFORM zf_mostra_coluna USING 'STATUS'               'Status'.

    IF <fs_fcat>-fieldname = 'PERC'.
      <fs_fcat>-do_sum = 'X'.
    ENDIF.

    IF <fs_fcat>-fieldname = 'MESANO'.
*      <fs_fcat>-no_out = 'X'.
      <fs_fcat>-tech   = 'X'.
    ENDIF.

    IF <fs_fcat>-fieldname = 'PERC_OLD'.
      <fs_fcat>-no_out = 'X'.
      <fs_fcat>-tech   = 'X'.
    ENDIF.

    IF <fs_fcat>-fieldname = 'KOKRS'.
*      <fs_fcat>-no_out = 'X'.
      <fs_fcat>-tech   = 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MOSTRA_COLUNA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1837   text
*      -->P_1838   text
*----------------------------------------------------------------------*
FORM zf_mostra_coluna    USING p_fieldname p_desc.

  IF <fs_fcat>-fieldname = p_fieldname.
    CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.
    <fs_fcat>-reptext = p_desc.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_header .

  DATA: l_document  TYPE REF TO cl_dd_document,
        l_doctable  TYPE REF TO cl_dd_table_element,
        l_column1   TYPE REF TO cl_dd_area,
        l_column2   TYPE REF TO cl_dd_area,
        l_text(255) TYPE c.  "Text

  CREATE OBJECT l_document.

* Titulo do Cabeçalho
  CALL METHOD l_document->add_text
    EXPORTING
      text      = 'Mês de Fechamento :'(013)
      sap_style = cl_dd_area=>heading.

  CLEAR : l_text.

  CONCATENATE  p_mesano+4(2) p_mesano(4) INTO l_text SEPARATED BY '/'.
  CALL METHOD l_document->add_text
    EXPORTING
      text      = l_text
      sap_style = cl_dd_area=>heading. "dEIXA EM NEGRITO

  CLEAR l_text.

************* - Titulo Legenda
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 150.

  CALL METHOD l_document->add_text
    EXPORTING
      text         = 'Legenda Status'(023)
      sap_emphasis = 'STRONG'.

  CALL METHOD l_document->new_line.

**************L1 - Legenda
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 230.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_CHECKED'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Pronto para Processamento'(024).

  CALL METHOD l_document->new_line.

**************L2 - Legenda
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 230.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_INCOMPLETE'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'O percentual total não fechou 100% para o Segmento'(025).

  CALL METHOD l_document->new_line.
**************L3 - Legenda
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 230.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_DEFECT'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Ordem não encontrada para este Ciclo e Segmento'(022).

  CALL METHOD l_document->new_line.
**************L4 - Legenda
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 230.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_LOCKED'.


  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Ordem Inconsistente\Bloqueada.Verif. dados mestres!'. "(026).

  CALL METHOD l_document->new_line.
**************L5 - Legenda
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 230.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_COMPLETE'.


  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Processamento efetuado com Sucesso'(026).

*********************************************
  CALL METHOD v_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 25.

  CALL METHOD l_document->display_document
    EXPORTING
      parent = v_container_1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SPLIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_split_screen .

  CLEAR: v_docking, v_splitter, v_container_1, v_container_2.
  CREATE OBJECT v_docking
    EXPORTING
      repid = sy-repid
      dynnr = sy-dynnr
      ratio = '95'.

* Create a splitter with 2 rows and 1 column
  CREATE OBJECT v_splitter
    EXPORTING
      parent  = v_docking
      rows    = 2
      columns = 1.

** Upper Container
  CALL METHOD v_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = v_container_1.

** Lower Container
  CALL METHOD v_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = v_container_2.

** Upper Container height

  CALL METHOD v_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 20.
*
  PERFORM zf_preparar_header.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ORDERNAR_SUBTOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_ordernar_subtotal CHANGING pt_sort TYPE lvc_t_sort.

  DATA: w_sort TYPE lvc_s_sort.

  CLEAR w_sort.
  w_sort-spos      = 1.
  w_sort-fieldname = 'CICLO'.
  APPEND w_sort TO pt_sort.

  CLEAR w_sort.
  w_sort-spos      = 2.
  w_sort-fieldname = 'SEGMENTO'.
  w_sort-subtot    = abap_true.
  APPEND w_sort TO pt_sort.

  CLEAR w_sort.
  w_sort-spos      = 3.
  w_sort-fieldname = 'SEGMENTO'.
  w_sort-subtot    = abap_true.
  APPEND w_sort TO pt_sort.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MOSTRA_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_mostra_mensagem.

  FIELD-SYMBOLS: <fs_upd>   LIKE LINE OF t_upd,
                 <fs_saida> LIKE LINE OF t_saida.

  READ TABLE t_bdcmsgcoll INTO w_bdcmsgcoll WITH KEY msgtyp = 'E'.
  IF sy-subrc IS INITIAL.
    MESSAGE ID w_bdcmsgcoll-msgid TYPE w_bdcmsgcoll-msgtyp NUMBER w_bdcmsgcoll-msgnr
        WITH w_bdcmsgcoll-msgv1 w_bdcmsgcoll-msgv2 w_bdcmsgcoll-msgv3 w_bdcmsgcoll-msgv4.
*    RETURN.

  ELSE.

    READ TABLE t_bdcmsgcoll INTO w_bdcmsgcoll WITH KEY msgtyp = 'I'.
    IF sy-subrc IS INITIAL.
      MESSAGE ID w_bdcmsgcoll-msgid TYPE w_bdcmsgcoll-msgtyp NUMBER w_bdcmsgcoll-msgnr
          WITH w_bdcmsgcoll-msgv1 w_bdcmsgcoll-msgv2 w_bdcmsgcoll-msgv3 w_bdcmsgcoll-msgv4.
*      RETURN.

    ELSE.

      READ TABLE t_bdcmsgcoll INTO w_bdcmsgcoll WITH KEY  msgid  = 'GA'
                                                          msgnr  = '013'
                                                          msgtyp = 'S'.
      IF sy-subrc IS INITIAL.

        LOOP AT t_upd ASSIGNING <fs_upd>.

          <fs_upd>-status    = 'P'.
          <fs_upd>-usnam     = sy-uname.
          <fs_upd>-zdt_atual = sy-datum.
          <fs_upd>-zhr_atual = sy-uzeit.

          READ TABLE t_saida ASSIGNING <fs_saida> WITH KEY mesano   = <fs_upd>-mesano
                                                           ciclo    = <fs_upd>-ciclo
                                                           segmento = <fs_upd>-segmento
                                                           receptor = <fs_upd>-receptor.

          IF sy-subrc IS INITIAL.
            <fs_saida>-status = w_icones-icon_proc.
          ENDIF.

        ENDLOOP.

        IF t_upd[] IS NOT INITIAL.
          MODIFY zcot0013 FROM TABLE t_upd.
        ENDIF.

        MESSAGE ID w_bdcmsgcoll-msgid TYPE w_bdcmsgcoll-msgtyp NUMBER w_bdcmsgcoll-msgnr
            WITH w_bdcmsgcoll-msgv1 w_bdcmsgcoll-msgv2 w_bdcmsgcoll-msgv3 w_bdcmsgcoll-msgv4.
        RETURN.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINAR_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_eliminar_registro .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida INTO w_saida INDEX w_row_no-row_id.

    IF w_saida-status = w_icones-icon_proc.
      MESSAGE s000(z_co)
        WITH 'Este registro não pode ser eliminado já foi '(021) 'processado.'
         DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      IF l_answer IS INITIAL.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'(017)
            text_question         = 'Deseja realmente eliminar os registros ?'(018)
            text_button_1         = 'Sim'(019)
            text_button_2         = 'Não'(020)
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = l_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

      ENDIF.

      CHECK l_answer = 1.
      READ TABLE  t_zcot0013 INTO w_zcot0013 WITH KEY mesano   = w_saida-mesano
                                                      ciclo    = w_saida-ciclo
                                                      segmento = w_saida-segmento
                                                      receptor = w_saida-receptor.

      IF sy-subrc IS INITIAL.
        APPEND w_zcot0013 TO t_delete.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF t_delete[] IS NOT INITIAL.
    DELETE zcot0013 FROM TABLE t_delete.
    COMMIT WORK.

    REFRESH: t_saida, t_zcot0013.
    PERFORM zf_selecionar_dados.
    PERFORM zf_definir_status .
    PERFORM zf_montar_saida.

    MESSAGE s000(z_co) WITH 'Registros eliminados!'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICAR_PORCENTAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_definir_status .

  FIELD-SYMBOLS <fs_zcot0013> LIKE LINE OF t_zcot0013.

  DATA: l_receptor       TYPE zcot0013-receptor,
        l_element1       TYPE c LENGTH 12,
        l_cycle          TYPE t811f-cycle,
        l_seg            TYPE c,
        l_aufnr          TYPE aufk-aufnr,
        l_error_occurred TYPE c.

  REFRESH: t_collect, t_t0013.

*----------------------------------------------------------------------*
*  Efetua somatória dos itens do Segment
*----------------------------------------------------------------------*
  LOOP AT t_zcot0013 INTO w_zcot0013.

    w_collect-ciclo     = w_zcot0013-ciclo.
    w_collect-segmento  = w_zcot0013-segmento.
    w_collect-perc      = w_zcot0013-perc.

    COLLECT w_collect INTO t_collect.

    MOVE-CORRESPONDING w_zcot0013 TO w_t0013.
    CONCATENATE p_kokrs w_t0013-ciclo INTO w_t0013-cycle.

    APPEND w_t0013 TO t_t0013.
    CLEAR w_t0013.

  ENDLOOP.

  PERFORM zf_buscar_dados_adicionais.

*----------------------------------------------------------------------*
*  Define status do processamento
*----------------------------------------------------------------------*
  LOOP AT t_zcot0013 ASSIGNING <fs_zcot0013>.

    CHECK <fs_zcot0013>-status <> 'P'.

    CLEAR w_collect.
    READ TABLE t_collect INTO w_collect WITH KEY segmento = <fs_zcot0013>-segmento
                                                 ciclo    = <fs_zcot0013>-ciclo.

    IF w_collect-perc <> '100.00'.
      <fs_zcot0013>-status = 'E'. "Erro    - Percentual abaixo de 100 - Icone = X vermelho
    ELSE.
      <fs_zcot0013>-status = 'X'. "Sucesso - Percentual = 100         - Icone = Check verde
    ENDIF.

* receptor não encontrado
    l_element1 = <fs_zcot0013>-receptor.
    UNPACK  l_element1 TO  l_element1.

* Monta Cycle
    CONCATENATE p_kokrs <fs_zcot0013>-ciclo INTO l_cycle.

    CLEAR w_t811f.
    READ TABLE  t_t811f INTO w_t811f WITH KEY  cycle    = l_cycle
                                               element1 = l_element1.
    IF sy-subrc IS NOT INITIAL.

      <fs_zcot0013>-status = 'D'. "Incompleto - Ordem não encontrada  - Bandeira Vermelha

    ELSE.


      LOOP AT  t_t811f INTO w_t811f  WHERE  cycle    = l_cycle
                                        AND element1 = l_element1. "<FS_ZCOT0013>-RECEPTOR.

* Segmento não encontrado
        READ TABLE t_t811s INTO w_t811s WITH KEY cycle = w_t811f-cycle
                                                 seqnr = w_t811f-seqnr
                                                 name  = <fs_zcot0013>-segmento.
        IF sy-subrc IS INITIAL.
          <fs_zcot0013>-perc_old  = w_t811f-value.
*          CONDENSE <fs_zcot0013>-perc_old.
          l_seg = 'X'.
          CONTINUE.
        ENDIF.

      ENDLOOP.

      IF l_seg IS INITIAL.
        <fs_zcot0013>-status = 'D'. "Erro
      ENDIF.

*----------------------------------------------------------------------*
      "'B' - Verifica o status da ordem
*----------------------------------------------------------------------*
      IF <fs_zcot0013>-status = 'X'.

        CLEAR: l_aufnr, l_error_occurred.

        l_aufnr = <fs_zcot0013>-receptor.
        UNPACK l_aufnr TO l_aufnr.

        "Cabeçalho do Ordem
        SELECT SINGLE * FROM aufkv
          INTO @DATA(w_aufkv)
          WHERE aufnr =  @l_aufnr.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'K_ORDER_CHECK'
            EXPORTING
              aufnr          = w_aufkv-aufnr "Nro. Ordem
              e_aufkv        = w_aufkv       "select single da ordem
              test_bukrs     = ''            "em branco
              test_kokrs     = <fs_zcot0013>-kokrs "centro de custo
              test_vrgng     = 'RKIV'
              actvt          = '02'          "03 não valida / 02 valida - passar
            IMPORTING
              error_occurred = l_error_occurred
            EXCEPTIONS
              OTHERS         = 1.

          IF sy-subrc IS NOT INITIAL.
            <fs_zcot0013>-status = 'B'.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_DADOS_ADICIONAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_buscar_dados_adicionais .

  CHECK t_zcot0013[] IS NOT INITIAL.

* Tabela de elementos de alocação
  SELECT * FROM t811f
    INTO TABLE t_t811f
    FOR ALL ENTRIES IN t_t0013
    WHERE cycle = t_t0013-cycle.

  SELECT * FROM t811k
    INTO TABLE t_t811k
   FOR ALL ENTRIES IN t_t0013
   WHERE cycle = t_t0013-cycle
     AND field = 'AUFNR'
     AND setid <> space.

  IF t_t811k[] IS NOT INITIAL.
    PERFORM zf_trata_registros_set.
  ENDIF.

* Segmentos de alocação
  IF t_t811f[] IS NOT INITIAL.

    SELECT * FROM t811s
      INTO TABLE t_t811s
      FOR ALL ENTRIES IN t_t811f
    WHERE cycle = t_t811f-cycle
      AND seqnr = t_t811f-seqnr.

* Texto descritivo - rateio/distribuição
    SELECT * FROM t811l
      INTO TABLE t_t811l
      FOR ALL ENTRIES IN t_t811f
    WHERE cycle = t_t811f-cycle
      AND seqnr = t_t811f-seqnr.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CONSISTIR_TELA_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_consistir_tela_selecao .

  IF r_arq IS NOT INITIAL.

    IF p_mesano IS NOT INITIAL.
      IF p_mesano+4(2) > 12.
        MESSAGE s000(z_co) WITH 'Mês\Ano incorreto!'(030) DISPLAY LIKE 'E'.
      ENDIF.
      IF p_mesano(4) < 2020.
        MESSAGE s000(z_co) WITH 'Mês\Ano incorreto!'(030) DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ENDIF.

  IF p_kokrs IS NOT INITIAL.

    CALL FUNCTION 'K_KOKRS_READ'
      EXPORTING
        kokrs  = p_kokrs
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.
      MESSAGE s000(z_co) WITH 'Área de contabilidade de custos'(028) p_kokrs 'não existe'(029)
                DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DADOS_COMPLEMENTARES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_dados_complementares .

  CHECK t_seg[] IS NOT INITIAL.

* Tabela de elementos de alocação
  SELECT * FROM t811f
    INTO TABLE t_t811f
    FOR ALL ENTRIES IN t_seg
    WHERE cycle = t_seg-cycle.

  IF t_t811f[] IS NOT INITIAL.

* Segmentos de alocação
    SELECT * FROM t811s
      INTO TABLE t_t811s
      FOR ALL ENTRIES IN t_t811f
    WHERE cycle = t_t811f-cycle
      AND seqnr = t_t811f-seqnr.

* Texto descritivo - rateio/distribuição
    SELECT * FROM t811l
      INTO TABLE t_t811l
      FOR ALL ENTRIES IN t_t811f
    WHERE cycle = t_t811f-cycle
      AND seqnr = t_t811f-seqnr.

  ENDIF.

  REFRESH: t_seg.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_validar_dados .

  FIELD-SYMBOLS: <fs_zcot0013> LIKE LINE OF t_zcot0013.

  DATA: l_receptor TYPE zcot0013-receptor,
        l_element1 TYPE c LENGTH 12,
        l_cycle    TYPE t811f-cycle.

  LOOP AT t_t811f INTO w_t811f.

* Segmentos de alocação
    READ TABLE t_t811s INTO w_t811s WITH KEY cycle = w_t811f-cycle
                                             seqnr = w_t811f-seqnr.

    READ TABLE t_zcot0013 INTO w_zcot0013 WITH KEY mesano   = p_mesano
                                                   ciclo    = w_t811f-cycle+4
                                                   segmento = w_t811s-name.

    CHECK sy-subrc IS INITIAL.

    SHIFT w_t811f-element1 LEFT DELETING LEADING '0'.
    CONDENSE w_t811f-element1.
    READ TABLE t_zcot0013 INTO w_zcot0013 WITH KEY mesano   = p_mesano
                                                   ciclo    = w_t811f-cycle+4
                                                   segmento = w_t811s-name
                                                   receptor = w_t811f-element1. "não faz parte da chave Z

    IF sy-subrc IS NOT INITIAL.

      CLEAR w_zcot0013.
      w_zcot0013-mesano	    =	p_mesano.
      w_zcot0013-ciclo      = w_t811f-cycle+4. "RETIRAR OS QUATROS PRIMEIROS CARACTERES)
      l_receptor            = w_t811f-element1.        "RETIRAR ZERO A ESQUERDA)

      SHIFT l_receptor LEFT DELETING LEADING '0'.
      CONDENSE l_receptor.

      w_zcot0013-receptor = l_receptor.

      w_zcot0013-perc       = '0.00'.   "GRAVAR 0,00
      w_zcot0013-usnam      = sy-uname. "USUÁRIO DE LOGIN
      w_zcot0013-zdt_atual  = sy-datum. "DATA DO SISTEMA
      w_zcot0013-zhr_atual  = sy-uzeit. "HORA DO SISTEMA

      w_zcot0013-segmento	  =	w_t811s-name.

* Texto descritivo - rateio/distribuição
      READ TABLE t_t811l INTO w_t811l WITH KEY cycle = w_t811f-cycle
                                               seqnr = w_t811f-seqnr.
      IF sy-subrc IS INITIAL.
        w_zcot0013-txt     = w_t811l-txt.
      ENDIF.

      APPEND w_zcot0013 TO t_zcot0013.

    ENDIF.

  ENDLOOP.

  PERFORM zf_definir_status.

  IF t_zcot0013[] IS NOT INITIAL.

    MESSAGE s000(z_co) WITH 'Dados carregados com sucesso.'(009).

    MODIFY zcot0013 FROM TABLE t_zcot0013.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATA_REGISTROS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_trata_registros_set .

  SORT t_t811k BY setid.
  DELETE ADJACENT DUPLICATES FROM t_t811k COMPARING cycle seqnr setid.

  LOOP AT t_t811k ASSIGNING FIELD-SYMBOL(<fs_t811k>).
    <fs_t811k>-setclass  = <fs_t811k>-setid(4).
    <fs_t811k>-setname2  = <fs_t811k>-setid+4.
  ENDLOOP.

  DATA(t_t811k_aux) = t_t811k.
  DELETE ADJACENT DUPLICATES FROM t_t811k_aux COMPARING setid.

* Conteúdo dos SET
  SELECT * FROM setleaf
    INTO TABLE @DATA(t_setleaf)
    FOR ALL ENTRIES IN @t_t811k_aux
    WHERE setclass = @t_t811k_aux-setclass
      AND setname  = @t_t811k_aux-setname2.

  LOOP AT t_t811k INTO DATA(w_t811k).

    LOOP AT t_setleaf INTO DATA(w_t_setleaf)
                              WHERE setclass = w_t811k-setclass
                                AND setname  = w_t811k-setname2.

      READ TABLE t_t811f INTO w_t811f WITH KEY cycle    = w_t811k-cycle
*                                              sdate    = w_t811k-sdate
                                              seqnr    = w_t811k-seqnr
                                              element1 = w_t_setleaf-valfrom.

      IF sy-subrc IS NOT INITIAL.

        w_t811f-tab      = w_t811k-tab.
        w_t811f-cycle    = w_t811k-cycle.
        w_t811f-sdate    = w_t811k-sdate.
        w_t811f-seqnr    = w_t811k-seqnr.
        w_t811f-element1 = w_t_setleaf-valfrom.
        w_t811f-value    = '0.00'.

        APPEND w_t811f TO t_t811f.
        CLEAR w_t811f.

      ENDIF.

    ENDLOOP.

  ENDLOOP.


*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_UPDATE_ZCOT0013
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_update_zcot0013 .

  DATA: t_fcat TYPE lvc_t_fcat.
  DATA: w_seg LIKE LINE OF t_seg.

  REFRESH: t_t811f, t_t811s, t_t811l, t_seg, t_zcot0013.
  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<w_saida>).

    IF <w_saida>-status <> w_icones-icon_proc.

      CLEAR <w_saida>-status .

      <w_saida>-usnam     = sy-uname.  "usuário de login
      <w_saida>-zdt_atual = sy-datum. "data do sistema
      <w_saida>-zhr_atual = sy-uzeit. "hora do sistema
      MOVE-CORRESPONDING <w_saida> TO w_seg.

      CONCATENATE p_kokrs w_seg-ciclo INTO w_seg-cycle.
      APPEND w_seg TO t_seg.

    ELSE.
      DELETE t_saida INDEX sy-tabix.
    ENDIF.

  ENDLOOP.

*----------------------------------------------------------------------*
* Move dados da tabela de saída para tabela Z
*----------------------------------------------------------------------*
  REFRESH: t_zcot0013.
  MOVE-CORRESPONDING t_saida TO t_zcot0013.
  REFRESH: t_saida.

  IF t_zcot0013[] IS NOT INITIAL.

    PERFORM zf_definir_status .

    MODIFY zcot0013 FROM TABLE t_zcot0013.
    COMMIT WORK.

    IF sy-subrc IS INITIAL.

* Importação Ciclo de Distribuição para KSV2
      IF t_zcot0013[] IS NOT INITIAL.
        PERFORM zf_selecionar_dados.
        PERFORM zf_montar_saida.
      ENDIF.

      IF v_grid IS NOT INITIAL.

        REFRESH: t_fcat[].
        CALL METHOD v_grid->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fcat[].

        LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
          <fs_fcat>-edit      = abap_false.
        ENDLOOP.

        CALL METHOD v_grid->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fcat[].

        CALL METHOD v_grid->refresh_table_display.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GERA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_arquivo .

  DATA valor(30).
  DATA: p_local   TYPE string,
        path(250).
  DATA: t_alvdata TYPE REF TO data.

  DATA:
    BEGIN OF t_fieldnames OCCURS 0,
      name TYPE char20,
    END OF t_fieldnames.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    IMPORTING
      filename         = path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc IS INITIAL.

    CONCATENATE path '.XLS' INTO p_local.

    t_fieldnames-name    = 'Ciclo'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Segmento'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Desc.Segmento'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Receptor'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Quota/percent.'.
    APPEND t_fieldnames.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename            = p_local
        filetype            = 'DBF'
      TABLES
        data_tab            = it_saida
        fieldnames          = t_fieldnames
      EXCEPTIONS
        file_open_error     = 1
        file_write_error    = 2
        invalid_filesize    = 3
        invalid_table_width = 4
        invalid_type        = 5
        no_batch            = 6
        unknown_error       = 7
        OTHERS              = 8.

    IF sy-subrc = 0.
      MESSAGE i000(z_co) WITH 'Arquivos gerados com sucesso.' DISPLAY LIKE 'S'.
    ELSE.
      MESSAGE i000(z_co) WITH 'Arquivo processado com erro.' DISPLAY LIKE 'E'..
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SF_SELEC_SEGMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sf_selec_segmento .

  DATA: w_zcot0013 LIKE LINE OF t_zcot0013.

*----------------------------------------------------------------------*
  "Buscar Ciclo, Segmento e Receptores
*----------------------------------------------------------------------*
  REFRESH: t_seg, t_t0013.


  IF p_ciclo IS NOT INITIAL.

    DATA l_cycle TYPE t811f-cycle.

    DATA(l_ciclo) = p_ciclo.

    REPLACE ALL OCCURRENCES OF '*' IN l_ciclo WITH '%'.

    CONCATENATE p_kokrs l_ciclo INTO l_cycle.

    SELECT DISTINCT cycle FROM t811f
      INTO TABLE @DATA(t_ciclo)
      WHERE cycle LIKE @l_cycle.

    CHECK t_ciclo[] IS NOT INITIAL.

    LOOP AT t_ciclo INTO DATA(w_ciclo).

      w_t0013-mesano = p_mesano.

      w_t0013-ciclo = w_ciclo-cycle+4.

      CHECK w_t0013-ciclo CP p_ciclo.

      w_t0013-cycle = w_ciclo-cycle.

      APPEND w_t0013 TO t_t0013.

      MOVE-CORRESPONDING w_t0013 TO w_zcot0013.
      APPEND w_zcot0013 TO t_zcot0013.

    ENDLOOP.

    IF t_zcot0013[] IS NOT INITIAL.
      PERFORM zf_buscar_dados_adicionais.
    ENDIF.

  ELSE.

    PERFORM zf_buscar_dados_adic_excel.

  ENDIF.

  LOOP AT  t_t811f INTO w_t811f. " WHERE  cycle    = <w_t0013>-cycle.

    ws_saida-ciclo    = w_t811f-cycle+4. "<w_t0013>-ciclo.
* Segmento não encontrado
    READ TABLE t_t811s INTO w_t811s WITH KEY cycle = w_t811f-cycle
                                             seqnr = w_t811f-seqnr.
    IF sy-subrc IS INITIAL.

      ws_saida-segmento = w_t811s-name.
      ws_saida-receptor = w_t811f-element1.

      "Busca Descrição Segmento
      READ TABLE t_t811l INTO w_t811l WITH KEY cycle = w_t811f-cycle
                                             seqnr = w_t811f-seqnr.
      IF w_t811l IS NOT INITIAL.
        ws_saida-des_seg = w_t811l-txt.
      ENDIF.

      APPEND ws_saida TO it_saida.

    ENDIF.

  ENDLOOP.

  SORT it_saida BY ciclo segmento.
  DELETE ADJACENT DUPLICATES FROM it_saida COMPARING ALL FIELDS.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATAR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_tratar_campos .

  LOOP AT SCREEN.

    IF r_arq IS NOT INITIAL.

      IF screen-group1 EQ 'VAL' OR screen-group1 EQ 'XLS'.
        screen-invisible = 1.
        screen-input     = 0.
      ENDIF.

    ENDIF.

    IF r_ksv2 IS NOT INITIAL.

      IF screen-group1 EQ 'VAL'.
        screen-invisible = 0.
        screen-input     = 1.
      ENDIF.

      IF r_grp IS NOT INITIAL.

        IF screen-group1 EQ 'XLS'.
          screen-invisible = 0.
          screen-input     = 1.
        ENDIF.

      ENDIF.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_DADOS_ADIC_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_buscar_dados_adic_excel .

  DATA(l_cycle) = p_kokrs && '%'.

* Tabela de elementos de alocação
  SELECT * FROM t811f
    INTO TABLE t_t811f
    WHERE cycle LIKE l_cycle.

* Campos-chave para alocação
  SELECT * FROM t811k
    INTO TABLE t_t811k
   FOR ALL ENTRIES IN t_t811f
   WHERE cycle = t_t811f-cycle
     AND field = 'AUFNR'
     AND setid <> space.

  IF t_t811k[] IS NOT INITIAL.
    PERFORM zf_trata_registros_set.
  ENDIF.

* Segmentos de alocação
  IF t_t811f[] IS NOT INITIAL.

    SELECT * FROM t811s
      INTO TABLE t_t811s
      FOR ALL ENTRIES IN t_t811f
    WHERE cycle = t_t811f-cycle
      AND seqnr = t_t811f-seqnr.

* Texto descritivo - rateio/distribuição
    SELECT * FROM t811l
      INTO TABLE t_t811l
      FOR ALL ENTRIES IN t_t811f
    WHERE cycle = t_t811f-cycle
      AND seqnr = t_t811f-seqnr.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_monta_tela .

  LOOP AT SCREEN.

    IF r_arq IS NOT INITIAL.

      IF screen-group1 EQ 'KSV'.
        screen-invisible = 1.
        screen-input     = 0.
      ENDIF.

    ENDIF.

    IF r_ksv2 IS NOT INITIAL.

      IF screen-group1 EQ 'KSV'.
        screen-invisible = 0.
        screen-input     = 1.
      ENDIF.

    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
