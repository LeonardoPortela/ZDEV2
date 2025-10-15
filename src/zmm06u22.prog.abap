*@#@@[SAP]
*&---------------------------------------------------------------------*
*& Report  ZMM06U22
*&
*&---------------------------------------------------------------------*

REPORT  zmm06u22 NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: dd02l, zmmt0003.

DATA: v_where(100),
      v_where2(100),
      v_where3(100),
      v_where4(100),
      v_set(100),
      v_erro(1),
      v_count TYPE i,
      i TYPE i.

DATA: dref TYPE REF TO data.
FIELD-SYMBOLS: <fs> TYPE ANY.
FIELD-SYMBOLS: <fx> TYPE ANY.
FIELD-SYMBOLS: <fy> TYPE ANY.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK bloc1 WITH FRAME TITLE text-001.
PARAMETERS:     p_table   TYPE dd03vt-tabname   OBLIGATORY.
PARAMETERS:     p_field   TYPE dd03vt-fieldname OBLIGATORY.
PARAMETERS:     p_valor   TYPE dd03vt-fieldname OBLIGATORY.
PARAMETERS:     p_field2  TYPE dd03vt-fieldname.
PARAMETERS:     p_valor2  TYPE dd03vt-fieldname.
PARAMETERS:     p_field3  TYPE dd03vt-fieldname.
PARAMETERS:     p_valor3  TYPE dd03vt-fieldname.


SELECTION-SCREEN SKIP 1.
PARAMETERS:     p_key01     TYPE dd03vt-fieldname OBLIGATORY.
PARAMETERS:     p_val01(50) TYPE c OBLIGATORY.
PARAMETERS:     p_key02     TYPE dd03vt-fieldname.
PARAMETERS:     p_val02(50) TYPE c.
PARAMETERS:     p_key03     TYPE dd03vt-fieldname.
PARAMETERS:     p_val03(50) TYPE c.
PARAMETERS:     p_key04     TYPE dd03vt-fieldname.
PARAMETERS:     p_val04(50) TYPE c.

SELECTION-SCREEN SKIP 1.
PARAMETERS:     p_test(1) TYPE c DEFAULT 'X'. "Teste
PARAMETERS:     p_dele(1) TYPE c .            "Delete
SELECTION-SCREEN END OF BLOCK bloc1.

********************************************************************

AT SELECTION-SCREEN.

  SELECT SINGLE *
  FROM dd02l
  INTO dd02l
  WHERE tabname = p_table.

  IF sy-subrc NE 0.

    MESSAGE e000(zmsg) WITH 'TABELA ' p_table ' NÃO EXISTE!'.

  ELSE.

    CREATE DATA dref TYPE (p_table).
    ASSIGN dref->* TO <fs>.
    IF sy-subrc NE 0.
      MESSAGE e000(zmsg) WITH 'TABELA ' p_table ' NÃO EXISTE!'.
    ENDIF.

    ASSIGN COMPONENT p_field OF STRUCTURE <fs> TO <fx>.
    IF sy-subrc NE 0.
      MESSAGE e000(zmsg) WITH 'Campo ' p_field ' NÃO EXISTE!'.
    ENDIF.

    IF p_field2 NE space.
      ASSIGN COMPONENT p_field2 OF STRUCTURE <fs> TO <fx>.
      IF sy-subrc NE 0.
        MESSAGE e000(zmsg) WITH 'Campo ' p_field2 ' NÃO EXISTE!'.
      ENDIF.
    ENDIF.

    IF p_field3 NE space.
      ASSIGN COMPONENT p_field3 OF STRUCTURE <fs> TO <fx>.
      IF sy-subrc NE 0.
        MESSAGE e000(zmsg) WITH 'Campo ' p_field3 ' NÃO EXISTE!'.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT p_key01 OF STRUCTURE <fs> TO <fx>.
    IF sy-subrc NE 0.
      MESSAGE e000(zmsg) WITH 'Campo chave 1 ' p_key01 ' NÃO EXISTE!'.
    ENDIF.

    IF NOT p_key02 IS INITIAL.
      ASSIGN COMPONENT p_key02 OF STRUCTURE <fs> TO <fx>.
      IF sy-subrc NE 0.
        MESSAGE e000(zmsg) WITH 'Campo chave 2 ' p_key02 ' NÃO EXISTE!'.
      ENDIF.
    ENDIF.

    IF NOT p_key03 IS INITIAL.
      ASSIGN COMPONENT p_key03 OF STRUCTURE <fs> TO <fx>.
      IF sy-subrc NE 0.
        MESSAGE e000(zmsg) WITH 'Campo chave 3 ' p_key03 ' NÃO EXISTE!'.
      ENDIF.
    ENDIF.

    IF NOT p_key04 IS INITIAL.
      ASSIGN COMPONENT p_key04 OF STRUCTURE <fs> TO <fx>.
      IF sy-subrc NE 0.
        MESSAGE e000(zmsg) WITH 'Campo chave 4 ' p_key03 ' NÃO EXISTE!'.
      ENDIF.
    ENDIF.

    IF p_val01 NE space AND p_key01 EQ space.
      MESSAGE e000(zmsg) WITH 'Preencher o Campo Chave 01.'.
    ENDIF.
    IF p_val02 NE space AND p_key02 EQ space.
      MESSAGE e000(zmsg) WITH 'Preencher o Campo Chave 02.'.
    ENDIF.
    IF p_val03 NE space AND p_key03 EQ space.
      MESSAGE e000(zmsg) WITH 'Preencher o Campo Chave 03.'.
    ENDIF.
    IF p_val04 NE space AND p_key04 EQ space.
      MESSAGE e000(zmsg) WITH 'Preencher o Campo Chave 04.'.
    ENDIF.
    IF p_valor2 NE space AND p_field2 EQ space.
      MESSAGE e000(zmsg) WITH 'Preencher o Campo Nome do campo 2.'.
    ENDIF.
    IF p_valor3 NE space AND p_field3 EQ space.
      MESSAGE e000(zmsg) WITH 'Preencher o Campo Nome do campo 3.'.
    ENDIF.

  ENDIF.

********************************************************************

START-OF-SELECTION.


  SELECT SINGLE *
  FROM zmmt0003
  INTO zmmt0003
  WHERE kostl = 'THOR'
    AND uname = sy-uname.

  IF sy-subrc = 0.

    v_count = STRLEN( p_valor ).
    CLEAR i.

    DO v_count TIMES.
      IF p_valor+i(1) CA '0123456789.'.
        v_erro = space.
      ELSE.
        v_erro = 'X'.
        EXIT.
      ENDIF.
      ADD 1 TO i.
    ENDDO.

    IF v_erro = space.
      CONCATENATE p_field ' = '  p_valor  INTO v_set SEPARATED BY space.
    ELSE.
      CONCATENATE p_field ' = ' ' ''' p_valor '''' INTO v_set.
    ENDIF.

    IF p_valor2 NE space.
      v_count = STRLEN( p_valor2 ).
      CLEAR: i, v_erro.

      DO v_count TIMES.
        IF p_valor2+i(1) CA '0123456789.'.
          v_erro = space.
        ELSE.
          v_erro = 'X'.
          EXIT.
        ENDIF.
        ADD 1 TO i.
      ENDDO.

      IF v_erro = space.
        IF p_field2 NE space.
          CONCATENATE  v_set p_field2 ' = '  p_valor2  INTO v_set SEPARATED BY space.
        ENDIF.
      ELSE.
        IF p_field2 NE space.
          CONCATENATE  v_set space p_field2 ' = ' ' ''' p_valor2 '''' INTO v_set.
        ENDIF.
      ENDIF.
    ENDIF.

    IF p_valor3 NE space.

      v_count = STRLEN( p_valor3 ).
      CLEAR: i, v_erro.

      DO v_count TIMES.
        IF p_valor3+i(1) CA '0123456789.'.
          v_erro = space.
        ELSE.
          v_erro = 'X'.
          EXIT.
        ENDIF.
        ADD 1 TO i.
      ENDDO.

      IF v_erro = space.
        IF p_field3 NE space.
          CONCATENATE  v_set p_field3 ' = '  p_valor3  INTO v_set SEPARATED BY space.
        ENDIF.
      ELSE.
        IF p_field3 NE space.
          CONCATENATE  v_set space p_field3 ' = ' ' ''' p_valor3 '''' INTO v_set.
        ENDIF.
      ENDIF.
    ENDIF.

    CONCATENATE p_key01 ' = ' ' ''' p_val01 '''' INTO v_where.

    IF p_key02 NE space.
      CONCATENATE p_key02 ' = ' ' ''' p_val02 '''' INTO v_where2.
    ENDIF.

    IF p_key03 NE space.
      CONCATENATE p_key03 ' = ' ' ''' p_val03 '''' INTO v_where3.
    ENDIF.

    IF p_key04 NE space.
      CONCATENATE p_key04 ' = ' ' ''' p_val04 '''' INTO v_where4.
    ENDIF.


    UNASSIGN <fs>.
    CREATE DATA dref TYPE (p_table).
    ASSIGN dref->* TO <fs>.

    CLEAR v_count.

    SELECT * "(p_field)
    FROM (p_table)
    INTO <fs>
    WHERE (v_where)
      AND (v_where2)
      AND (v_where3)
      AND (v_where4).

      ADD 1 TO v_count.

      IF p_test EQ space.

*        IF v_count GT 0.

        IF p_dele = space.

          UNASSIGN <fx>.
          ASSIGN COMPONENT p_field OF STRUCTURE <fs> TO <fx>.
          <fx> = p_valor.

          IF p_field2 NE space.
            UNASSIGN <fy>.
            ASSIGN COMPONENT p_field2 OF STRUCTURE <fs> TO <fy>.
            <fy> = p_valor2.
          ENDIF.

          IF p_field3 NE space.
            UNASSIGN <fy>.
            ASSIGN COMPONENT p_field3 OF STRUCTURE <fs> TO <fy>.
            <fy> = p_valor3.
          ENDIF.

          UPDATE (p_table) FROM <fs>.

          IF sy-subrc = 0.
            WRITE: / 'MARRETA EFETUADA COM SUCESSO NA TABELA:', p_table.
            WRITE: / 'Foram encontrados ', v_count, ' registro(s).'.
            CLEAR v_count.
          ELSE.
            WRITE: / 'Houve um erro durante o Update.'.
            CLEAR v_count.
          ENDIF.

        ELSE.

          DELETE FROM (p_table)
          WHERE (v_where)
              AND (v_where2)
              AND (v_where3)
              AND (v_where4).

          IF sy-subrc = 0.
            WRITE: / 'MARRETA EFETUADA COM SUCESSO NA TABELA:', p_table.
            WRITE: / 'Foram encontrados ', v_count, ' registro(s) e eliminados'.
            CLEAR v_count.
          ELSE.
            WRITE: / 'Houve um erro durante o Update.'.
            CLEAR v_count.
          ENDIF.

        ENDIF.

      ELSE.  "p_test EQ space.

        IF v_count GT 0.
          WRITE: / 'TESTE EFETUADO COM SUCESSO PARA A TABELA:', p_table.
          WRITE: / 'CAMPOS ENTRADOS SÃO CONSISTENTES.'.
          WRITE: / 'Foram encontrados ', v_count, ' registro(s).'.
          CLEAR v_count.
        ELSE.
          WRITE: / 'TESTE EFETUADO COM SUCESSO PARA A TABELA:', p_table.
          WRITE: / 'CAMPOS ENTRADOS SÃO CONSISTENTES.'.
          SKIP.
          WRITE: / 'Mas não foram encontrados registros!'.
          CLEAR v_count.
        ENDIF.

      ENDIF.

    ENDSELECT.

    IF sy-subrc NE 0.
      WRITE: / 'NAO FORAM ENCONTRADOS DADOS COM ESSAS CHAVES.'.
    ENDIF.

  ELSE.

    WRITE: / 'SEM PERMISSAO NA TAB ZMMT0003'.

  ENDIF.

END-OF-SELECTION.
