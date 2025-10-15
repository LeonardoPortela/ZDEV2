FUNCTION Z_APROV_LIBERA_ORDEM_PM.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_USER) TYPE  USNAM
*"     REFERENCE(I_WERKS) TYPE  WERKS_EXT
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_TCODE) TYPE  SYTCODE
*"     REFERENCE(I_WAERS) TYPE  WAERS
*"     REFERENCE(I_VALOR) TYPE  CAUFV-USER4
*"  EXPORTING
*"     REFERENCE(E_AUTORIZA_ORCA) TYPE  INTEGER
*"     REFERENCE(E_AUTORIZA_SUPL) TYPE  INTEGER
*"----------------------------------------------------------------------
  DATA: GT_002  TYPE TABLE OF ZPMR0002 WITH HEADER LINE.

  FIELD-SYMBOLS: <FS_002> TYPE ZPMR0002.

  REFRESH: GT_002.
  CLEAR: GT_002.

  E_AUTORIZA_ORCA = 1.
  E_AUTORIZA_SUPL = 1.

  CASE I_TCODE.
    WHEN 'KO24'.
      SELECT *
        FROM ZPMR0002
        INTO TABLE GT_002
       WHERE BUKRS       = I_BUKRS
         AND APROVADOR   = I_USER
         AND SUPL_ORC    = 'X'
         AND WAERS       = I_WAERS.

    WHEN 'KO22' OR 'IPM2'.
      SELECT *
        FROM ZPMR0002
        INTO TABLE GT_002
       WHERE BUKRS       = I_BUKRS
         AND APROVADOR   = I_USER
         AND WAERS       = I_WAERS.

  ENDCASE.

**  Checa apenas centro solicitado
  DELETE GT_002 WHERE CENTRO_DESP NS I_WERKS.

**  Checa se há permissão de orçamento para o usuário de acordo com o valor
  LOOP AT GT_002 ASSIGNING <FS_002>.
    IF  I_VALOR < <FS_002>-VALOR_DE.
      ADD 1 TO E_AUTORIZA_ORCA.
    ELSEIF I_VALOR > <FS_002>-VALOR_ATE
    AND <FS_002>-VALOR_ATE NE ''.
      ADD 1 TO E_AUTORIZA_ORCA.
    ELSE.
      CLEAR E_AUTORIZA_ORCA.

**  Checa se há permissão de suplementação para o usuário
      IF <FS_002>-SUPL_ORC = 'X'.
        CLEAR E_AUTORIZA_SUPL.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFUNCTION.
