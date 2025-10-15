class ZCL_NF_REMOVEDOR_TMISS definition
  public
  final
  create public .

public section.

  types:
    ty_r_nftype TYPE RANGE OF J_1BNFDOC-NFTYPE .

  constants C_TVARV_NFTYPE type TVARV-NAME value 'ZSD_NFTYPE_TMISS' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !I_S_DOC_HEADER type J_1BNFDOC .
  methods INICIAR
    changing
      !C_T_DOC_ITEM type TY_J_1BNFLIN .
protected section.
private section.

  data S_DOC_HEADER type J_1BNFDOC .
  data R_NFTYPE type TY_R_NFTYPE .

  methods BUSCAR_CONSTANTES .
  methods VERIFICAR_VALIDO
    returning
      value(R_V_VALIDO) type BOOLEAN .
  methods VERIFICAR_NFTYPE
    raising
      CX_J1BNFE_INVALID_VALUE .
ENDCLASS.



CLASS ZCL_NF_REMOVEDOR_TMISS IMPLEMENTATION.


  METHOD buscar_constantes.

    SELECT sign,
           opti AS option,
           low,
           high
    INTO TABLE @r_nftype
      FROM tvarvc
      WHERE name = @me->c_tvarv_nftype.

  ENDMETHOD.


  METHOD constructor.

    me->s_doc_header = i_s_doc_header.

    me->buscar_constantes( ).

  ENDMETHOD.


  METHOD iniciar.

    IF me->verificar_valido( ) IS NOT INITIAL.

      LOOP AT c_t_doc_item ASSIGNING FIELD-SYMBOL(<fs_doc_item>).

        CLEAR <fs_doc_item>-tmiss.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD verificar_nftype.

    IF  me->r_nftype            IS NOT INITIAL
    AND me->s_doc_header-nftype IN me->r_nftype.

      RETURN.

    ENDIF.

    RAISE EXCEPTION TYPE cx_j1bnfe_invalid_value.

  ENDMETHOD.


  METHOD verificar_valido.

    TRY.

        r_v_valido = abap_true.

        me->verificar_nftype( ).

      CATCH cx_root.

        r_v_valido = abap_false.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
