*&---------------------------------------------------------------------*
*&  Include           ZXXXX01
*&---------------------------------------------------------------------*
    DATA:
      tlg_esq_calc TYPE TABLE OF zsds006 WITH HEADER LINE,
      BEGIN OF l_vars OCCURS 0,
        var(50) TYPE c,
      END OF l_vars.

    DATA: tg_tabelas   TYPE TABLE OF zsds012 WITH HEADER LINE,
          tg_workareas TYPE TABLE OF zsds012 WITH HEADER LINE,
          tg_code      TYPE TABLE OF zsds011 WITH HEADER LINE.

    DATA:prog(8),
         msg(120),
         lin(3),
         wrd(10),
         off(3).

    CONSTANTS: c_c   VALUE 'C',
               c_r   VALUE 'R',
               c_tam TYPE i VALUE 350.

    DATA:
      code(c_tam) OCCURS 0.

    DATA:
      exp(c_tam) TYPE c,
      tipo_calc  TYPE zsdt0070-tipo_calc,
      wg_fixacao TYPE zsdt0059-posnr,
      repid      TYPE sy-repid,
      c_decimais TYPE zsdt0070-c_decimais.

*  FIELD-SYMBOLS: <FS_TABLE2> TYPE STANDARD TABLE.
*&---------------------------------------------------------------------*
*&      Form  CALCULA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
    FORM calcula  TABLES tl_esq_calc STRUCTURE zsds006
                  USING p_esq_calc TYPE zsds006
                        i_fixacao  TYPE posnr
                        i_repid  TYPE sy-repid
                  CHANGING p_result ."TYPE BSID-DMBTR.


      REFRESH: tlg_esq_calc, code, l_vars.
      tlg_esq_calc[] = tl_esq_calc[].

      exp = p_esq_calc-formula.
      repid = i_repid.
      c_decimais = p_esq_calc-c_decimais.
      tipo_calc = p_esq_calc-tipo_calc.
      PERFORM create_program.
      prog = 'ZCALC'.

      EXPORT c_decimais TO MEMORY ID 'C_DECIMAIS'.
      EXPORT code TO MEMORY ID 'CODE'.
*    EXPORT i_repid TO MEMORY ID 'I_REPID'.
      EXPORT tlg_esq_calc TO MEMORY ID 'ESQ_CALC'.
      wg_fixacao = i_fixacao.
      EXPORT wg_fixacao TO MEMORY ID 'WG_FIXACAO'.
      SUBMIT zsdr0025
      AND RETURN.

      IMPORT <fs_result> TO p_result FROM MEMORY ID 'RESULT'.

    ENDFORM.                    "calcula
*&---------------------------------------------------------------------*
*&      Form  create_program
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

    FORM create_program .
      DATA: line(c_tam),
            wl_field(30).

      APPEND 'PROGRAM SUBPOOL.' TO code.
      PERFORM declare_variables.

*    LINE =  'FORM DYN1 changing p_result(13) type P decimals ' && C_DECIMAIS.
*    CONCATENATE 'FORM DYN1 changing p_result(13) type P decimals' C_DECIMAIS INTO LINE SEPARATED BY SPACE.
*    APPEND LINE TO CODE.
      APPEND 'FORM DYN1 changing p_result.' TO code.

      IF tipo_calc EQ 'R'.
        APPEND `FIELD-SYMBOLS: <FS_TABLE2> TYPE STANDARD TABLE.` TO code.

        LOOP AT tg_tabelas.
          CONCATENATE `ASSIGN` tg_tabelas-componente INTO line SEPARATED BY space.
          CONCATENATE line `[]` INTO line.
          CONCATENATE line `TO <FS_TABLE2>.` INTO line SEPARATED BY space.
          APPEND line TO code.
*      APPEND `ASSIGN TG_ITENS[] TO <FS_TABLE2>.` TO CODE.
          CONCATENATE `IMPORT <FS_TABLE2> FROM MEMORY ID '`tg_tabelas-componente `'.` INTO line.
          APPEND line TO code.
*      APPEND `IMPORT <FS_TABLE2> FROM MEMORY ID 'TG_ITENS_FS'.` TO CODE.
          CONCATENATE tg_tabelas-componente '[] = <FS_TABLE2>.' INTO line.
          APPEND line TO code.
          APPEND 'UNASSIGN <FS_TABLE2>.' TO code.
        ENDLOOP.
        APPEND `IMPORT WG_FIXACAO FROM MEMORY ID 'WG_FIXACAO'.` TO code.
        LOOP AT tg_code.
          line = tg_code-line.
          APPEND line TO code.
          CLEAR line.
        ENDLOOP.

*    APPEND `IMPORT TG_ITENS FROM MEMORY ID 'TG_ITENS'.` TO CODE.
      ELSE.
        line = 'ASSIGN (''(ZSDR0025)TL_ESQ_CALC[]'') TO <FS_TABLE>.'.
        APPEND line TO code.

        line = 'TL_ESQ_CALC[] = <FS_TABLE>.'.
        APPEND line TO code.

        LOOP AT l_vars.
          READ TABLE tlg_esq_calc
            WITH KEY nivel = l_vars-var.
          CONCATENATE 'v' l_vars INTO wl_field.

          CONCATENATE 'READ TABLE TL_ESQ_CALC WITH KEY NIVEL = ''' l_vars-var '''.'
              INTO line SEPARATED BY space.
          APPEND line TO code.
          APPEND 'CONDENSE TL_ESQ_CALC-FORMULA NO-GAPS.' TO code.
          APPEND 'TRY.' TO code.
          CONCATENATE wl_field ' = TL_ESQ_CALC-FORMULA.'
            INTO line SEPARATED BY space.
          APPEND line TO code.
          APPEND 'catch  CX_SY_CONVERSION_NO_NUMBER.' TO code.
          APPEND 'ENDTRY.' TO code.
        ENDLOOP.

        APPEND 'Try.' TO code.
        CONCATENATE 'p_result ='
                    exp
                    '.'
                    INTO line SEPARATED BY space.
        APPEND line TO code.
        APPEND 'catch CX_SY_ZERODIVIDE.' TO code.
        APPEND 'endTry.' TO code.
      ENDIF.
      APPEND 'ENDFORM.'
              TO code.


    ENDFORM.                    " create_program
*&---------------------------------------------------------------------*
*&      Form  declare_variables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

    FORM declare_variables.
*    TYPES: BEGIN OF TY_ITENS,
*        STATUS(4),
*        POSNR     TYPE ZSDT0053-POSNR,
*        FIXACAO   TYPE ZSDT0053-FIXACAO,
*        MATNR     TYPE ZSDT0053-MATNR,
*        MAKTX     TYPE MAKT-MAKTX,
*        WERKS     TYPE ZSDT0053-WERKS,
*        PONTO_C   TYPE ZSDT0053-PONTO_C,
*        TERMINAL  TYPE ZSDT0053-TERMINAL,
*        LGORT     TYPE ZSDT0053-LGORT,
*        CHARG     TYPE ZSDT0053-CHARG,
*        ZMENG     TYPE ZSDT0053-ZMENG,
*        BRGEW     TYPE ZSDT0053-BRGEW,
*        ZIEME     TYPE ZSDT0053-ZIEME,
*        DMBTR     TYPE ZSDT0053-DMBTR,
*        VLRTOT    TYPE ZSDT0053-VLRTOT,
*        VOLUM     TYPE ZSDT0053-VOLUM,
*        VOLEH     TYPE ZSDT0053-VOLEH,
*        PMEIN     TYPE ZSDT0053-PMEIN,
*        KURSF     TYPE ZSDT0053-KURSF,
*        VALDT     TYPE ZSDT0053-VALDT,
*        VBELN     TYPE ZSDT0053-VBELN,
*        ITEM_EDIT TYPE ZSDT0053-ITEM_EDIT,
*        KUNNR     TYPE ZSDT0053-KUNNR,
*        STATUS_ITM TYPE ZSDT0053-STATUS_ITM,
*        STYLE     TYPE LVC_T_STYL,
*        LINE_COLOR(4) TYPE C,
*       END OF TY_ITENS.
      DATA : BEGIN OF tl_exp OCCURS 0,
               field(c_tam),
             END OF tl_exp.

      DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
            lr_datadescr   TYPE REF TO cl_abap_datadescr,
            lr_elemdescr   TYPE REF TO cl_abap_elemdescr.
      DATA: lt_components TYPE abap_component_tab,
            l_component   LIKE LINE OF lt_components.
      DATA l_field_name TYPE fieldname.

      DATA:l_exp(c_tam)     TYPE c,
           line(c_tam)      TYPE c,
           formula          TYPE emg_trdata,
           wl_field(30),
           wl_field_aux(30),
           wl_field_fs(30),
           wl_1(100),
           wl_2(100),


           l_value(20)      TYPE c,
           l_type.
*    TYPES: TY_TC_RESGATE     TYPE STANDARD TABLE OF TY_ITENS.

      FIELD-SYMBOLS:
        <fs_table>  TYPE ANY TABLE,
        <fs_table2> TYPE STANDARD TABLE,
        <fs_line>   TYPE any.
*    BREAK-POINT.
      l_exp = exp.
      CONDENSE l_exp NO-GAPS.
      IF tipo_calc EQ c_c.
        TRANSLATE l_exp USING '(|)|+|-|/|*|'.
        SPLIT l_exp AT '|' INTO TABLE l_vars.
        DELETE l_vars WHERE var IS INITIAL.

        APPEND 'DATA: TL_ESQ_CALC TYPE TABLE OF ZSDS006 WITH HEADER LINE.' TO code.
        line = 'FIELD-SYMBOLS: <FS_TABLE> TYPE ANY TABLE.'.
        APPEND line TO code.

        REFRESH: tl_exp.
        SPLIT exp AT space INTO TABLE tl_exp.
        SORT: l_vars BY var.
        DELETE ADJACENT DUPLICATES FROM l_vars COMPARING var.
        LOOP AT l_vars.
          READ TABLE tlg_esq_calc
            WITH KEY nivel = l_vars-var.
          SHIFT tlg_esq_calc-formula LEFT DELETING LEADING space.
          l_value = tlg_esq_calc-formula.

          CONCATENATE 'v' l_vars '(13)' INTO wl_field.
          CONCATENATE 'v' l_vars INTO wl_field_aux.

          CONCATENATE 'data:'
                      wl_field
                      'type P decimals'  " 'type BSID-DMBTR'
                      tlg_esq_calc-c_decimais
                      '.'
                      INTO line SEPARATED BY space.

          APPEND line TO code.

*      REPLACE ALL OCCURRENCES OF L_VARS-VAR IN TABLE TL_EXP WITH WL_FIELD_AUX .
          LOOP AT tl_exp
            WHERE field EQ l_vars-var.

            tl_exp-field = wl_field_aux.
            MODIFY tl_exp.
          ENDLOOP.


        ENDLOOP.

        LOOP AT tl_exp.
          IF sy-tabix EQ 1.
            exp = tl_exp-field.
          ELSE.
            CONCATENATE exp tl_exp-field INTO exp SEPARATED BY space.
          ENDIF.
        ENDLOOP.
      ELSE.
*      BREAK-POINT.
        formula = exp.
        CALL FUNCTION 'ZSDMF009_INTERPRETA_COD_CALC'
          EXPORTING
            i_formula             = formula
          TABLES
            te_code               = tg_code
            te_tabelas            = tg_tabelas
            te_workareas          = tg_workareas
          EXCEPTIONS
            formula_inconsistente = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        APPEND `DATA: WG_FIXACAO TYPE ZSDT0059-POSNR.` TO code.
        LOOP AT tg_tabelas.
          CONCATENATE '(' repid ')'tg_tabelas-componente'[]' INTO wl_field_fs.
*      ASSIGN ('(ZSDR0022)TG_ITENS[]') TO <FS_TABLE>.
          ASSIGN (wl_field_fs) TO <fs_table>.
          LOOP AT <fs_table> ASSIGNING <fs_line>.
            lr_structdescr ?= cl_abap_typedescr=>describe_by_data( <fs_line> ).
            lt_components = lr_structdescr->get_components( ).
            EXIT.
          ENDLOOP.
          LOOP AT lt_components INTO l_component.
            CLEAR: line.
            IF sy-tabix EQ 1.
              CONCATENATE 'TYPES: BEGIN OF TY_' tg_tabelas-componente ',' INTO line.
              APPEND line TO code.
*          APPEND 'TYPES: BEGIN OF TY_ITENS,' TO CODE.
            ENDIF.
            lr_datadescr = l_component-type.
            IF l_component-as_include = space.
              SPLIT lr_datadescr->absolute_name AT '=' INTO wl_1
                                                            wl_2.
              IF wl_1 EQ '\TYPE'.
                IF wl_2(2) NE '%_'.
                  CONCATENATE l_component-name 'TYPE' wl_2',' INTO line SEPARATED BY space.
                  APPEND line TO code.
                ELSE.
                  lr_elemdescr ?= lr_datadescr.
                  wl_2 = lr_elemdescr->output_length.
                  CONDENSE wl_2 NO-GAPS.
                  CONCATENATE l_component-name'(' wl_2 ')' INTO line.
                  CONCATENATE line 'TYPE' lr_datadescr->type_kind',' INTO line SEPARATED BY space.
                  APPEND line TO code.
                ENDIF.
              ENDIF.
            ENDIF.
            AT LAST.
              CONCATENATE 'END OF TY_' tg_tabelas-componente '.' INTO line.
              APPEND line TO code.
              CONCATENATE 'DATA:' tg_tabelas-componente 'TYPE TABLE OF TY_' INTO line SEPARATED BY space.
              CONCATENATE line tg_tabelas-componente INTO line.
              CONCATENATE line 'WITH HEADER LINE.' INTO line SEPARATED BY space.
              APPEND line TO code.
*          APPEND '       END OF TY_ITENS.' TO CODE.
*          APPEND 'DATA: TG_ITENS TYPE TABLE OF TY_ITENS WITH HEADER LINE.' TO CODE.


              ASSIGN <fs_table> TO <fs_table2>.
              EXPORT <fs_table2> TO MEMORY ID tg_tabelas-componente.

            ENDAT.
          ENDLOOP.
        ENDLOOP.

      ENDIF.

    ENDFORM.                    " declare_variables
