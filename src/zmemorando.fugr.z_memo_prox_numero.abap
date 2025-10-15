FUNCTION z_memo_prox_numero.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DIRECAO) TYPE  Z_MEMO_DIRECAO DEFAULT '1'
*"     REFERENCE(REMETENTE) TYPE  LIFNR OPTIONAL
*"     REFERENCE(REPRESENTANTE) TYPE  KUNNR OPTIONAL
*"  EXPORTING
*"     VALUE(NUMERO) TYPE  Z_MEMO_NUMERO
*"  EXCEPTIONS
*"      LOCAL_NEGOCIO
*"      NUMERO_MEMO
*"----------------------------------------------------------------------

  DATA: wa_j_1bbranch     TYPE j_1bbranch,
        vg_branch         TYPE j_1bbranc_,
        wa_zdoc_memo_nr   TYPE zdoc_memo_nr,
        vg_numero_valido  TYPE c LENGTH 1,
        wa_zdoc_memo_nr_u TYPE zdoc_memo_nr_u.

  "Quando 2 (saída) a numeração deve ser do memorando recebido pela maggi do terceiro.
  CHECK direcao EQ 1.

  CASE direcao.
    WHEN 1. "Entrada
      "Quando direção for entrada o representante do "produtor" sempre
      "será um local de negócio
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = representante
        IMPORTING
          output = vg_branch.
    WHEN 2. "Saída
      "Quando direção for saída o remetente da mercadoria sempre será
      "um local de negócio
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = remetente
        IMPORTING
          output = vg_branch.
  ENDCASE.

  SELECT SINGLE * INTO wa_j_1bbranch
    FROM j_1bbranch
   WHERE branch EQ vg_branch.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e059 RAISING local_negocio WITH vg_branch.
  ENDIF.

  SELECT SINGLE * INTO wa_zdoc_memo_nr
    FROM zdoc_memo_nr
   WHERE bukrs  EQ wa_j_1bbranch-bukrs
     AND branch	EQ wa_j_1bbranch-branch.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e060 RAISING numero_memo WITH wa_j_1bbranch-bukrs wa_j_1bbranch-branch.
  ELSE.

    IF wa_zdoc_memo_nr-numero_prox IS INITIAL.
      numero = wa_zdoc_memo_nr-numero_memo.
    ELSE.
      numero = wa_zdoc_memo_nr-numero_prox.
    ENDIF.

    CLEAR: vg_numero_valido.

    WHILE vg_numero_valido IS INITIAL.

      SELECT SINGLE * INTO wa_zdoc_memo_nr_u
        FROM zdoc_memo_nr_u
       WHERE bukrs      EQ wa_j_1bbranch-bukrs
         AND branch     EQ wa_j_1bbranch-branch
         AND numero     EQ numero.

      IF NOT sy-subrc IS INITIAL.
        vg_numero_valido = 'X'.
        wa_zdoc_memo_nr-numero_prox = numero + 1.
      ELSE.
        numero = numero + 1.
      ENDIF.

    ENDWHILE.

    MODIFY zdoc_memo_nr FROM wa_zdoc_memo_nr.

    COMMIT WORK.

  ENDIF.

ENDFUNCTION.
