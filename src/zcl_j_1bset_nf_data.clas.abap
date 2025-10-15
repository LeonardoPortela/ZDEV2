class ZCL_J_1BSET_NF_DATA definition
  public
  final
  create public .

public section.

  types:
    TYT_J1BNFLIN TYPE TABLE OF J_1BNFLIN .

  class-methods CHANGE_NF_HEADER
    importing
      !I_IT_ITEM type TY_J_1BNFLIN
    changing
      value(C_DOCHEADER) type J_1BNFDOC .
  class-methods CHANGE_NF_ITEM
    importing
      value(I_DOCHEADER) type J_1BNFDOC
    changing
      !C_IT_ITEM type TY_J_1BNFLIN .
  class-methods CHANGE_NF
    changing
      !C_DOCHEADER type J_1BNFDOC
      !C_IT_ITEM_OBJ type ZTY_J_1BNFLIN optional
      !C_IT_ITEM type TY_J_1BNFLIN optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_J_1BSET_NF_DATA IMPLEMENTATION.


  method CHANGE_NF.

     LOOP AT c_it_item_obj ASSIGNING FIELD-SYMBOL(<fs_item>).

*---------------------------------------------------------------------------------------*
*     ICMS Monofasico - Ini Descomentar apos equalização Requests
*---------------------------------------------------------------------------------------*


      CASE <fs_item>-taxsit.

       WHEN 'E'.

          SELECT SINGLE *
            FROM zrsi_icms_mono
            INTO @DATA(ls_icms_mono)
           WHERE ncm      EQ @<fs_item>-nbm
             AND matnr    EQ @<fs_item>-matnr.

          IF NOT sy-subrc IS INITIAL.

            SELECT SINGLE *
              FROM zrsi_icms_mono
              INTO ls_icms_mono
             WHERE ncm      EQ <fs_item>-nbm
               AND matnr    EQ space.

          ENDIF.

          IF sy-subrc IS INITIAL.

            <fs_item>-p_dif = ls_icms_mono-p_dif.
            <fs_item>-q_bc_mono = <fs_item>-menge.
            <fs_item>-adrem_icms = ls_icms_mono-amount.
            <fs_item>-v_icms_mono_op = <fs_item>-q_bc_mono * <fs_item>-adrem_icms.
            <fs_item>-v_icms_mono_dif = <fs_item>-v_icms_mono_op * ( ls_icms_mono-p_dif / 100 ).
            <fs_item>-v_icms_mono = <fs_item>-v_icms_mono_op - <fs_item>-v_icms_mono_dif.

          ENDIF.

        WHEN 'F'.

          SELECT SINGLE *
            FROM zrsi_icms_mono
            INTO ls_icms_mono
           WHERE ncm      EQ <fs_item>-nbm
             AND matnr    EQ <fs_item>-matnr.

          IF NOT sy-subrc IS INITIAL.

            SELECT SINGLE *
              FROM zrsi_icms_mono
              INTO ls_icms_mono
             WHERE ncm      EQ <fs_item>-nbm
               AND matnr    EQ space.

          ENDIF.

          IF sy-subrc IS INITIAL.

            IF ls_icms_mono-factor IS INITIAL.
              ls_icms_mono-factor = 1.
            ENDIF.

            <fs_item>-adrem_icms_ret  = ls_icms_mono-amount.
            <fs_item>-v_icms_mono_ret = ls_icms_mono-amount * <fs_item>-menge.

          ENDIF.

        WHEN OTHERS.

      ENDCASE.

*---------------------------------------------------------------------------------------*
*     ICMS Monofasico - Ini
*---------------------------------------------------------------------------------------*
"
   ENDLOOP.

  endmethod.


  method CHANGE_NF_HEADER.


    c_docheader-ind_intermed = '0'.

    IF c_docheader-ind_pres NE '2' and
       c_docheader-ind_pres NE '3' and
       c_docheader-ind_pres NE '4' and
       c_docheader-ind_pres NE '9'.
      clear c_docheader-ind_intermed .
    ENDIF.


  endmethod.


  METHOD change_nf_item.

  "  LOOP AT c_it_item ASSIGNING FIELD-SYMBOL(<fs_item>).

*---------------------------------------------------------------------------------------*
*     ICMS Monofasico - Ini Descomentar apos equalização Requests
*---------------------------------------------------------------------------------------*


*      CASE <fs_item>-taxsit.
*
*        WHEN 'E'.
*
*          SELECT SINGLE *
*            FROM zrsi_icms_mono
*            INTO @DATA(ls_icms_mono)
*           WHERE ncm      EQ @<fs_item>-nbm
*             AND matnr    EQ @<fs_item>-matnr.
*
*          IF NOT sy-subrc IS INITIAL.
*
*            SELECT SINGLE *
*              FROM zrsi_icms_mono
*              INTO ls_icms_mono
*             WHERE ncm      EQ <fs_item>-nbm
*               AND matnr    EQ space.
*
*          ENDIF.
*
*          IF sy-subrc IS INITIAL.
*
*            <fs_item>-p_dif = ls_icms_mono-p_dif.
*            <fs_item>-q_bc_mono = <fs_item>-menge.
*            <fs_item>-adrem_icms = ls_icms_mono-amount.
*            <fs_item>-v_icms_mono_op = <fs_item>-q_bc_mono * <fs_item>-adrem_icms.
*            <fs_item>-v_icms_mono_dif = <fs_item>-v_icms_mono_op * ( ls_icms_mono-p_dif / 100 ).
*            <fs_item>-v_icms_mono = <fs_item>-v_icms_mono_op - <fs_item>-v_icms_mono_dif.
*
*          ENDIF.
*
*        WHEN 'F'.
*
*          SELECT SINGLE *
*            FROM zrsi_icms_mono
*            INTO ls_icms_mono
*           WHERE ncm      EQ <fs_item>-nbm
*             AND matnr    EQ <fs_item>-matnr.
*
*          IF NOT sy-subrc IS INITIAL.
*
*            SELECT SINGLE *
*              FROM zrsi_icms_mono
*              INTO ls_icms_mono
*             WHERE ncm      EQ <fs_item>-nbm
*               AND matnr    EQ space.
*
*          ENDIF.
*
*          IF sy-subrc IS INITIAL.
*
*            IF ls_icms_mono-factor IS INITIAL.
*              ls_icms_mono-factor = 1.
*            ENDIF.
*
*            <fs_item>-adrem_icms_ret  = ls_icms_mono-amount.
*            <fs_item>-v_icms_mono_ret = ls_icms_mono-amount * <fs_item>-menge.
*
*          ENDIF.
*
*        WHEN OTHERS.
*
*      ENDCASE.

*---------------------------------------------------------------------------------------*
*     ICMS Monofasico - Ini
*---------------------------------------------------------------------------------------*

   " ENDLOOP.

  ENDMETHOD.
ENDCLASS.
