*&---------------------------------------------------------------------*&
*& Report  ZPMR0016                                                    *&
*&                                                                     *&
*& Descrição....: Sistema p/ Empréstimo de equipamentos                *&
*& Analista.....: Cleudo Ferreira                                      *&
*& Desenvolvedor: Anderson Oenning.                                    *&
*& Data 01/11/2018.                                                    *&
*&                                                                     *&
*& Modulo.......: PM          Transação: ZPM0026                       *&
*&---------------------------------------------------------------------*&


  INCLUDE ZPMR0016_TOPO.
  INCLUDE ZPMR0016_CLASS.
  INCLUDE ZPMR0016_FORMS.
  INCLUDE ZPMR0016_PBO.
  INCLUDE ZPMR0016_PAI.
*&---------------------------------------------------------------------*
*&      Form  ORDERNA_EQUI_PARA_TRANSF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORDERNA_EQUI_PARA_TRANSF .

*     Adicionando por prioridade.
      DATA: CONT    TYPE P DECIMALS 1,
            LT_EQUZ TYPE EQUZ.

      CLEAR CONT.

      LOOP AT IT_SAIDA_EMPRESTIMO_EQUI ASSIGNING FIELD-SYMBOL(<TR_EQUI>).
        <TR_EQUI>-EQUNR = |{ <TR_EQUI>-EQUNR ALPHA = IN }|.
        CLEAR: LT_EQUZ.
        SELECT SINGLE *
        FROM EQUZ AS A
        INNER JOIN EQUI AS B ON B~EQUNR EQ A~EQUNR
        INTO CORRESPONDING FIELDS OF LT_EQUZ
          WHERE A~EQUNR EQ <TR_EQUI>-EQUNR
            AND A~DATBI EQ '99991231'
            AND B~EQTYP IN (  'A', 'V', '1', '2', '3', '4' ). "FF - 22/11/20023 e 05/04/2024 type A - ins

        ADD 1 TO CONT.

        IF LT_EQUZ-EQUNR IS NOT INITIAL AND LT_EQUZ-HEQUI IS INITIAL .
          <TR_EQUI>-SEQUENCIA = 1.
          CONTINUE.
        ENDIF.

        IF LT_EQUZ-EQUNR IS NOT INITIAL AND LT_EQUZ-HEQUI IS NOT INITIAL.
          <TR_EQUI>-SEQUENCIA = 2.
          CONTINUE.
        ENDIF.

        IF LT_EQUZ IS INITIAL.
          <TR_EQUI>-SEQUENCIA = CONT.
        ENDIF.
      ENDLOOP.

      SORT IT_SAIDA_EMPRESTIMO_EQUI ASCENDING BY SEQUENCIA.

ENDFORM.
