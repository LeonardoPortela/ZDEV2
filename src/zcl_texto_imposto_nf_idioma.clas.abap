class ZCL_TEXTO_IMPOSTO_NF_IDIOMA definition
  public
  final
  create public .

public section.

  constants C_TVARV_CPROG type TVARV-NAME value 'ZSD_TXT_NF_IMPOSTO_PROGRAMA' ##NO_TEXT.
  constants C_TVARV_DIRECT type TVARV-NAME value 'ZSD_TXT_NF_IMPOSTO_DIR_MOV' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !I_W_J_1BNFDOC type J_1BNFDOC .
  methods ATRIBUIR_IDIOMA
    changing
      !C_W_J_1BNFDOC type J_1BNFDOC .
protected section.
private section.

  types:
    ty_r_cprog  TYPE RANGE OF sy-cprog .
  types:
    ty_r_direct TYPE RANGE OF j_1bnfdoc-direct .

  data W_J_1BNFDOC type J_1BNFDOC .
  data R_CPROG type TY_R_CPROG .
  data R_DIRECT type TY_R_DIRECT .

  methods BUSCAR_CONSTANTES .
  methods VERIFICAR_VALIDO
    returning
      value(R_V_VALIDO) type BOOLEAN .
  methods VERIFICAR_PROGRAMA
    raising
      CX_SY_PROGRAM_NOT_FOUND .
  methods VERIFICAR_DIRECT
    raising
      CX_J1BNFE_INVALID_VALUE .
ENDCLASS.



CLASS ZCL_TEXTO_IMPOSTO_NF_IDIOMA IMPLEMENTATION.


  METHOD atribuir_idioma.

    IF me->verificar_valido( ) IS NOT INITIAL.

      c_w_j_1bnfdoc-spras_bupla = 'P'.

    ENDIF.

  ENDMETHOD.


  METHOD buscar_constantes.

    SELECT sign,
           opti AS option,
           low,
           high
    INTO TABLE @r_cprog
      FROM tvarvc
      WHERE name = @me->c_tvarv_cprog.

    SELECT sign,
           opti AS option,
           low,
           high
    INTO TABLE @r_direct
      FROM tvarvc
      WHERE name = @me->c_tvarv_direct.

  ENDMETHOD.


  METHOD constructor.

    me->buscar_constantes( ).
    me->w_j_1bnfdoc = i_w_j_1bnfdoc.

  ENDMETHOD.


  METHOD verificar_direct.

    IF  me->r_direct           IS NOT INITIAL
    AND me->w_j_1bnfdoc-direct IN me->r_direct.

      RETURN.

    ENDIF.

    RAISE EXCEPTION TYPE cx_j1bnfe_invalid_value.

  ENDMETHOD.


  METHOD verificar_programa.

    IF  me->r_cprog IS NOT INITIAL
    AND sy-cprog    IN me->r_cprog.

      RETURN.

    ENDIF.

    RAISE EXCEPTION TYPE cx_sy_program_not_found.

  ENDMETHOD.


  method VERIFICAR_VALIDO.

    TRY.

      r_v_valido = abap_true.

      me->verificar_programa( ).
      me->verificar_direct( ).

    CATCH cx_root.

      r_v_valido = abap_false.

    ENDTRY.

  endmethod.
ENDCLASS.
