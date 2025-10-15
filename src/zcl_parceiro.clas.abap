class ZCL_PARCEIRO definition
  public
  final
  create public .

public section.

  class-methods GET_PARCEIRO_LOCAL_NEGOCIO
    importing
      !I_PARTINER type LIFNR
    exporting
      !E_J_1BBRANCH type J_1BBRANCH
    returning
      value(R_LOCAL_NEGOCIO) type CHAR01 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PARCEIRO IMPLEMENTATION.


  METHOD get_parceiro_local_negocio.

    DATA: lc_partiner TYPE lifnr.

    CLEAR: e_j_1bbranch, r_local_negocio.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = i_partiner
      IMPORTING
        output = lc_partiner.

    DATA(qtd_caracter) = strlen( lc_partiner ).

    IF qtd_caracter LE 4.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lc_partiner
        IMPORTING
          output = lc_partiner.

      e_j_1bbranch-branch = lc_partiner+6(4).

      SELECT SINGLE * INTO e_j_1bbranch
        FROM j_1bbranch
       WHERE branch EQ e_j_1bbranch-branch.

      IF sy-subrc IS INITIAL.
        r_local_negocio = abap_true.
      ELSE.
        CLEAR: e_j_1bbranch.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
