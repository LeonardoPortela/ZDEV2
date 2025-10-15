*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 06/06/2010                                              &*
*& Descrição: Relatório de investimento                               &*
*& Transação: ZIM08                                                   &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

REPORT  ZIM08_NEW.

TABLES: T001, IMAK, ZIM08_REL_INV_US, BSID.

DATA: WG_VISAO TYPE ZIM08_REL_INV2-VISAO.

DATA: BEGIN OF T_EMPRESA OCCURS 0.
        INCLUDE STRUCTURE T001.
DATA: END OF T_EMPRESA.

***********************************************************************
***********************************************************************
*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-s03.
*PARAMETERS: p_consol RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND xcom,
*            p_analit RADIOBUTTON GROUP g1,
*            p_sincax RADIOBUTTON GROUP g1.
*SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
PARAMETERS:   P_VISAO  TYPE ZIM08_REL_INV2-VISAO,
              P_IMED(1),
              p_teste(1).
SELECT-OPTIONS: S_BUKRS  FOR T001-BUKRS      NO-EXTENSION,
                S_POSNR  FOR IMAK-POSNR,
                S_GJAHR  FOR IMAK-GJAHR,
                S_WAERS  FOR ZIM08_REL_INV_US-WAERS,
                S_MONAT  FOR BSID-MONAT.
SELECTION-SCREEN END OF BLOCK B1.
***********************************************************************
***********************************************************************

START-OF-SELECTION.
  DATA: WL_CONSOL,
        WL_ANALIT,
        WL_SINCAX.

  WHILE p_teste is not INITIAL.

  ENDWHILE.


  SELECT * FROM T001 INTO TABLE T_EMPRESA." WHERE bukrs = s_bukrs.

  DELETE T_EMPRESA WHERE NOT BUKRS IN S_BUKRS.

*  PERFORM z_determina_visao CHANGING wg_visao.
  IF P_IMED IS INITIAL.
    READ TABLE S_WAERS
      WITH KEY LOW = 'BRL'.

    IF S_WAERS[] IS INITIAL
    OR SY-SUBRC IS INITIAL.

      DELETE FROM ZIM08_REL_INV2 WHERE  VISAO  EQ P_VISAO AND
                                       ( GJAHR IN S_GJAHR
                                      OR GJAHR EQ '0000' )
                                        AND ABUKRS IN S_BUKRS
                                        AND POSNR IN S_POSNR.
    ENDIF.


    READ TABLE S_WAERS
     WITH KEY LOW = 'USD'.

    IF S_WAERS[] IS INITIAL
    OR SY-SUBRC IS INITIAL.
      DELETE FROM ZIM08_REL_INV_US WHERE VISAO EQ P_VISAO AND
                                         ( GJAHR IN S_GJAHR
                                        OR GJAHR EQ '0000'
                                        OR GJAHR EQ SPACE )
                                        AND ABUKRS IN S_BUKRS
                                        AND POSNR  IN S_POSNR.
    ENDIF.
  ENDIF.

  DATA: WL_MOEDA TYPE ZIM01_MOEDA,
        WL_TABIX(1).

  RANGES: R_BUKRS FOR T001-BUKRS.
  LOOP AT T_EMPRESA.
    CLEAR R_BUKRS.
    REFRESH R_BUKRS.

    R_BUKRS-SIGN = 'I'.
    R_BUKRS-OPTION = 'EQ'.
    R_BUKRS-LOW = T_EMPRESA-BUKRS.
    APPEND R_BUKRS.
    CLEAR WL_TABIX.
*    DO 2 TIMES.

*    ADD 1 TO WL_TABIX.
*    IF WL_TABIX = '1'.
*      WL_MOEDA = 'BRL'.
*    ELSE.
*      WL_MOEDA = 'USD'.
*    ENDIF.


*      SUBMIT ZIM01 WITH P_ON     EQ 'X'
*                   WITH P_CONSOL EQ 'X'
*                   WITH P_ANALIT EQ ''
*                   WITH P_SINCAX EQ ''
*                   WITH S_BUKRS  IN R_BUKRS
*                   WITH S_POSNR  IN S_POSNR
*                   WITH S_GJAHR  IN S_GJAHR
*                   WITH P_MOEDA  EQ WL_MOEDA
*      AND RETURN.
*
*
*      SUBMIT ZIM01 WITH P_ON     EQ 'X'
*                   WITH P_CONSOL EQ ''
*                   WITH P_ANALIT EQ 'X'
*                   WITH P_SINCAX EQ ''
*                   WITH S_BUKRS  IN R_BUKRS
*                   WITH S_POSNR  IN S_POSNR
*                   WITH S_GJAHR  IN S_GJAHR
*                   WITH P_MOEDA  EQ WL_MOEDA
*      AND RETURN.
    CASE P_VISAO.
      WHEN '01'.
        WL_CONSOL = 'X'.
      WHEN '02'.
        WL_ANALIT = 'X'.
      WHEN '03'.
        WL_SINCAX = 'X'.
    ENDCASE.
    IF S_WAERS[] IS NOT INITIAL.
      READ TABLE S_WAERS INDEX 1.
      WL_MOEDA = S_WAERS-LOW.

      SUBMIT ZIM01 WITH P_ON     EQ 'X'
                 WITH P_CONSOL EQ WL_CONSOL
                 WITH P_ANALIT EQ WL_ANALIT
                 WITH P_SINCAX EQ WL_SINCAX
                 WITH S_BUKRS  IN R_BUKRS
                 WITH S_POSNR  IN S_POSNR
                 WITH S_GJAHR  IN S_GJAHR
                 WITH P_MOEDA  EQ WL_MOEDA
                 WITH P_CARGA  EQ 'X'
    AND RETURN.
    ELSE.
      WL_MOEDA = 'BRL'.
      SUBMIT ZIM01 WITH P_ON     EQ 'X'
                 WITH P_CONSOL EQ WL_CONSOL
                 WITH P_ANALIT EQ WL_ANALIT
                 WITH P_SINCAX EQ WL_SINCAX
                 WITH S_BUKRS  IN R_BUKRS
                 WITH S_POSNR  IN S_POSNR
                 WITH S_GJAHR  IN S_GJAHR
                 WITH P_MOEDA  EQ WL_MOEDA
                 WITH P_CARGA  EQ 'X'
    AND RETURN.

      WL_MOEDA = 'USD'.
      SUBMIT ZIM01 WITH P_ON     EQ 'X'
                 WITH P_CONSOL EQ WL_CONSOL
                 WITH P_ANALIT EQ WL_ANALIT
                 WITH P_SINCAX EQ WL_SINCAX
                 WITH S_BUKRS  IN R_BUKRS
                 WITH S_POSNR  IN S_POSNR
                 WITH S_GJAHR  IN S_GJAHR
                 WITH P_MOEDA  EQ WL_MOEDA
                 WITH P_CARGA  EQ 'X'
    AND RETURN.
    ENDIF.
*    ENDDO.
  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  Z_DETERMINA_VISAO
*&---------------------------------------------------------------------*
*FORM z_determina_visao  CHANGING ch_visao.
*
*  CASE 'X'.
*
*    WHEN p_consol.
*      ch_visao = '01'.
*    WHEN p_analit.
*      ch_visao = '02'.
*    WHEN p_sincax .
*      ch_visao = '03'.
*  ENDCASE.
*
*  DELETE FROM zim08_rel_inv2 WHERE visao ne '99'.
*
*ENDFORM.                    " Z_DETERMINA_VISAO
