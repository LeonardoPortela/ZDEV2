class Z_FI_UTIL definition
  public
  final
  create public .

public section.

  class-methods GET_PARTIDAS_ABERTO_FORNECEDOR
    importing
      !I_COMPANY type BUKRS
      !I_FORNECEDOR type LIFNR
    exporting
      !IT_BSXS type ZDE_BSIS_T
      !IT_BSXK type ZDE_BSIK_T
      !IT_BKPF type ZDE_BKPF_T
      !IT_BSEG type ZDE_BSEG_T .
  class-methods GET_SALDO_ADIANTAMENTO_VIAGEM
    importing
      !I_COMPANY type BUKRS
      !I_FORNECEDOR type LIFNR
    exporting
      !E_DMBTR type DMBTR
      !E_DMBE2 type DMBE2
      !E_DMBE3 type DMBE3 .
protected section.
private section.
ENDCLASS.



CLASS Z_FI_UTIL IMPLEMENTATION.


  METHOD GET_PARTIDAS_ABERTO_FORNECEDOR.

    CALL FUNCTION 'Z_FI_GL_PART_ABERTO'
      EXPORTING
        I_COMPANY = I_COMPANY
        I_FORNE   = ABAP_TRUE
        I_PARID   = I_FORNECEDOR
      TABLES
        IT_BSXS   = IT_BSXS
        IT_BSXK   = IT_BSXK
        IT_BKPF   = IT_BKPF
        IT_BSEG   = IT_BSEG.

  ENDMETHOD.


  METHOD GET_SALDO_ADIANTAMENTO_VIAGEM.

    DATA: "IT_BSXS  TYPE ZDE_BSIS_T,
      IT_BSXK	TYPE ZDE_BSIK_T,
      "IT_BKPF  TYPE ZDE_BKPF_T,
      "IT_BSEG  TYPE ZDE_BSEG_T,
      "WA_BKPF TYPE BKPF,
      WA_BSIK TYPE BSIK.

    E_DMBTR = 0.
    E_DMBE2 = 0.
    E_DMBE3 = 0.

    CALL METHOD Z_FI_UTIL=>GET_PARTIDAS_ABERTO_FORNECEDOR
      EXPORTING
        I_COMPANY    = I_COMPANY
        I_FORNECEDOR = I_FORNECEDOR
      IMPORTING
        IT_BSXK      = IT_BSXK.

    LOOP AT IT_BSXK INTO WA_BSIK.

      ADD WA_BSIK-DMBTR TO E_DMBTR.
      ADD WA_BSIK-DMBE2 TO E_DMBE2.
      ADD WA_BSIK-DMBE3 TO E_DMBE3.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
