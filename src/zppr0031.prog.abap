*&---------------------------------------------------------------------*
*& Report ZPPR0031
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPPR0031.

*-------------------------------
* Declaração dos parâmetros
*-------------------------------
PARAMETERS: p_werks  TYPE t001l-werks OBLIGATORY,
            p_lgort1 TYPE t001l-lgort OBLIGATORY,
            p_lgort2 TYPE t001l-lgort OBLIGATORY.

*-------------------------------
* Tabelas internas e variáveis
*-------------------------------
DATA: lt_new_t001l    TYPE STANDARD TABLE OF t001l,
      ls_t001l        TYPE t001l,
      lv_lgort_num(4) TYPE c,
      lv_lgort        TYPE t001l-lgort,
      lv_desc         TYPE t001l-lgobe.

*-------------------------------
* Validações iniciais
*-------------------------------
START-OF-SELECTION.

  " Verificar se depósitos são numéricos
  IF NOT p_lgort1 CO '0123456789' AND NOT p_lgort2 CO '0123456789'.
    MESSAGE 'Depósitos alfanuméricos não são permitidos.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " Verificar se o depósito inicial é menor ou igual ao final
  IF p_lgort1 > p_lgort2.
    MESSAGE 'Depósito Inicial deve ser menor ou igual ao Depósito Final' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM t001w
    INTO @DATA(ls_t001w)
    WHERE werks = @p_werks.
  IF sy-subrc <> 0.
    MESSAGE 'Centro informado não existe.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-------------------------------
* Loop para gerar dados novos
*-------------------------------
  DATA(lv_inicio) = p_lgort1.
  DATA(lv_fim)    = p_lgort2.

  DO ( lv_fim - lv_inicio + 1 ) TIMES.
    lv_lgort_num = lv_inicio + sy-index - 1.
    CONDENSE lv_lgort_num NO-GAPS.
    lv_lgort = |{ lv_lgort_num ALPHA = IN }|.

    " Verifica se já existe na T001L
    SELECT SINGLE * FROM t001l
      INTO @DATA(ls_exist)
      WHERE werks = @p_werks
        AND lgort = @lv_lgort.

    IF sy-subrc <> 0.

      CLEAR ls_t001l.
      ls_t001l-werks = p_werks.
      ls_t001l-lgort = lv_lgort.

      ls_t001l-lgobe = |Bloco { lv_lgort }|.

      APPEND ls_t001l TO lt_new_t001l.

    ENDIF.

  ENDDO.

*-------------------------------
* Inserção na tabela T001L
*-------------------------------
  IF lt_new_t001l IS NOT INITIAL.

    DATA(lv_answer) = 'N'.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption = 'N'
        textline1     = |{ lines( lt_new_t001l ) } depósitos serão criados.|
        textline2     = 'Deseja continuar?'
        titel         = 'Confirmação'
      IMPORTING
        answer        = lv_answer.

    IF lv_answer = 'J'. " 'J' significa Sim em sistemas SAP PT-BR
      INSERT t001l FROM TABLE lt_new_t001l.
      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE |{ lines( lt_new_t001l ) } depósitos criados com sucesso.| TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE 'Erro ao inserir depósitos.' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE 'Operação cancelada.' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE 'Nenhum depósito novo foi inserido. Todos já existem.' TYPE 'I'.
    EXIT.
  ENDIF.
