FUNCTION zpm_listar_suplementos1.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(USERNAME) TYPE  SY-UNAME
*"  TABLES
*"      T_SUPLEMENTOS STRUCTURE  ZPMR0006 OPTIONAL
*"----------------------------------------------------------------------

  TYPES : BEGIN OF ty_suplementos.
            INCLUDE TYPE zpmr0006.
  TYPES :
*  types :   nivel      type numc10,
            aprovador  TYPE usnam,
            permit     TYPE pmsog,
            usua_subst TYPE usnam,
            data_lim   TYPE sy-datum,
            check      TYPE char1,
          END OF ty_suplementos.

  DATA: wa_zpmr0002 TYPE zpmr0002.

  TYPES: BEGIN OF ty_ordens2.
           INCLUDE STRUCTURE zpmr0006.
  TYPES:   kostl     TYPE kostl,
           aprovador TYPE bname,
         END OF ty_ordens2.

  DATA: suplementos  TYPE TABLE OF ty_suplementos,
        it_zpmr0006  TYPE TABLE OF zpmr0006 WITH DEFAULT KEY,
        estrategias  TYPE TABLE OF zpmr0002,
        vl_nivel     TYPE numc10,
        user_data    TYPE alm_me_user_data,
        order_header TYPE alm_me_order_header,
        user_profile TYPE alm_me_c010prf,
        rg_aufnr_e   TYPE RANGE OF aufnr,
        rg_aufnr_d   TYPE RANGE OF aufnr,
        t_ordens2    TYPE STANDARD TABLE OF ty_ordens2,
        r_werks      TYPE rsis_t_range,
        w_ordens2    TYPE ty_ordens2.

  DATA: gt_zpmr0002 TYPE TABLE OF zpmr0002 WITH HEADER LINE,
        gt_zpmr0011 TYPE TABLE OF zpmr0011 WITH HEADER LINE.
  DATA: zt_zpmr0002 TYPE TABLE OF zpmr0002 WITH HEADER LINE.

  FREE: gt_zpmr0011, gt_zpmr0002.
  SELECT *
   FROM zpmr0002
   INTO CORRESPONDING FIELDS OF TABLE gt_zpmr0002
   WHERE aprovador  EQ sy-uname
      OR usua_subst EQ sy-uname. "Anderson Oenning
  IF sy-subrc NE 0.
    SELECT *
  FROM zpmr0011
  INTO CORRESPONDING FIELDS OF TABLE gt_zpmr0011
  WHERE aprovador  EQ sy-uname
     OR usua_subst EQ sy-uname. "Anderson Oenning
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING gt_zpmr0011[] TO gt_zpmr0002[].
    ENDIF.
  ENDIF.

  LOOP AT gt_zpmr0002 ASSIGNING FIELD-SYMBOL(<wa_estrategia>).
    IF <wa_estrategia>-aprovador IS NOT INITIAL
      AND <wa_estrategia>-usua_subst IS NOT INITIAL
      AND <wa_estrategia>-data_lim >= sy-datum.
      <wa_estrategia>-aprovador = <wa_estrategia>-usua_subst.
    ENDIF.
  ENDLOOP.


  SELECT DISTINCT a~mandt a~aufnr a~belnr a~status a~solicitante a~dt_solicitacao a~vlr_estimado a~werks
        a~observacao a~responsavel a~dt_modificacao a~equipment a~equipment_desc a~short_text a~object
        a~currency a~nivel_aprovado a~obs_reprov c~nivel c~aprovador c~permit c~usua_subst c~data_lim  "Anderson Oenning
  FROM zpmr0006 AS a
  INNER JOIN aufk AS b ON b~aufnr EQ a~aufnr
  INNER JOIN zpmr0002 AS c ON c~centro_desp EQ a~werks
  INTO CORRESPONDING FIELDS OF TABLE suplementos
  WHERE b~phas1 EQ abap_true
   AND  a~status NE 'L'.

*  if sy-subrc ne 0.
  SELECT DISTINCT a~mandt a~aufnr a~belnr a~status a~solicitante a~dt_solicitacao a~vlr_estimado a~werks
        a~observacao a~responsavel a~dt_modificacao a~equipment a~equipment_desc a~short_text a~object
        a~currency a~nivel_aprovado a~obs_reprov c~nivel c~aprovador c~permit c~usua_subst c~data_lim  "Anderson Oenning
  FROM zpmr0006 AS a
  INNER JOIN aufk AS b ON b~aufnr EQ a~aufnr
  INNER JOIN zpmr0011 AS c ON c~centro_desp EQ a~werks
  APPENDING CORRESPONDING FIELDS OF TABLE suplementos
  WHERE b~phas1 EQ abap_true
   AND  a~status NE 'L'.
*  endif.

  SORT suplementos BY object nivel aprovador.
  DELETE ADJACENT DUPLICATES FROM suplementos COMPARING object nivel aprovador.

  LOOP AT suplementos ASSIGNING FIELD-SYMBOL(<supl>) WHERE check IS INITIAL.
    vl_nivel = <supl>-nivel_aprovado.

*    IF VL_NIVEL IS INITIAL.
*      <SUPL>-CHECK = ABAP_TRUE.
*
*      MODIFY SUPLEMENTOS
*        FROM <SUPL>
*          TRANSPORTING CHECK
*            WHERE OBJECT EQ <SUPL>-OBJECT.
*
*
*      <SUPL>-CHECK = ABAP_FALSE.
*
*      MODIFY SUPLEMENTOS
*        FROM <SUPL>
*          TRANSPORTING CHECK
*              WHERE OBJECT EQ <SUPL>-OBJECT
*                 AND NIVEL EQ <SUPL>-NIVEL
*                AND PERMIT EQ <SUPL>-PERMIT.

*      ADD 1 TO VL_NIVEL.
*      IF VL_NIVEL NE <SUPL>-NIVEL.
*        <SUPL>-CHECK = ABAP_TRUE.
*      ENDIF.

*    ELSE.
    ADD 1 TO vl_nivel.
    IF vl_nivel NE <supl>-nivel.
      <supl>-check = abap_true.
    ENDIF.
*    ENDIF.
  ENDLOOP.

  LOOP AT suplementos ASSIGNING <supl>.
    IF <supl>-aufnr IS NOT INITIAL
      AND <supl>-aprovador IS NOT INITIAL
      AND <supl>-usua_subst IS NOT INITIAL
      AND <supl>-data_lim >= sy-datum.
      <supl>-aprovador = <supl>-usua_subst.
    ENDIF.
  ENDLOOP.

  DELETE suplementos WHERE check IS NOT INITIAL.

  "FF #185560 - inicio

  "//Tabela de permissões p/ solicitação de suplemento;
  SELECT *
  FROM zpmr0007 FOR ALL ENTRIES IN @suplementos
  WHERE usnam = @sy-uname
    AND werks = @suplementos-werks
  INTO TABLE @DATA(lt_zpmr0007).

  IF lt_zpmr0007[] IS NOT INITIAL.

    DATA lr_werks TYPE RANGE OF zpmr0007-werks.

    lr_werks = VALUE #(
      FOR wa IN lt_zpmr0007
      ( sign = 'I' option = 'EQ' low = wa-werks )
    ).

    DELETE suplementos WHERE status <> 'R'.

    IF lr_werks IS NOT INITIAL.
      DELETE suplementos WHERE werks NOT IN lr_werks.
    ENDIF.

  ELSE.
    "FF #185560 - fim

    DELETE suplementos WHERE aprovador NE username.

  ENDIF.

  it_zpmr0006 = VALUE #( FOR ls IN suplementos ( CORRESPONDING #( ls ) ) ).


*  <<<<<<<
  LOOP AT it_zpmr0006[] ASSIGNING FIELD-SYMBOL(<fs_ordens>).
    MOVE-CORRESPONDING <fs_ordens> TO w_ordens2.
    SELECT SINGLE kostl FROM aufk WHERE aufnr = @<fs_ordens>-aufnr INTO @w_ordens2-kostl.
    IF sy-subrc = 0.
      SELECT * FROM zpmr0011
        WHERE kostl = @w_ordens2-kostl
       AND aprovador  EQ @sy-uname
       AND nivel      <= @<fs_ordens>-nivel
        OR usua_subst EQ @sy-uname
            INTO TABLE @DATA(it_zpmr0011).
      IF sy-subrc = 0.

        APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ordens>-aufnr ) TO rg_aufnr_e[].
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ordens>-werks ) TO r_werks[].

      ELSE.

        "Verifica se para esse centro de custo existe aprovador.
        SELECT SINGLE * FROM zpmr0011
          INTO @DATA(ws_zpmr0011)
        WHERE kostl EQ @w_ordens2-kostl
          AND nivel <= @<fs_ordens>-nivel.
        IF sy-subrc EQ 0.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ordens>-aufnr ) TO rg_aufnr_d[].
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF rg_aufnr_e[] IS NOT INITIAL.
    DELETE it_zpmr0006 WHERE aufnr NOT IN rg_aufnr_e[] AND werks IN r_werks[].
  ENDIF.

  IF rg_aufnr_d[] IS NOT INITIAL.
    DELETE it_zpmr0006 WHERE aufnr IN rg_aufnr_d[].
  ENDIF.

  APPEND LINES OF it_zpmr0006 TO t_suplementos.
  delete ADJACENT DUPLICATES FROM t_suplementos COMPARING ALL FIELDS. "FF #185560

ENDFUNCTION.
