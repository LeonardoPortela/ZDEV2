*&---------------------------------------------------------------------*
*&  Include           ZXEQMU07
*&---------------------------------------------------------------------*


TYPES: BEGIN OF ty_zcat_ordem,
         param TYPE ztparam-param,
         zval  TYPE t370t-eqtyp,
         const TYPE t370u-typtx,
       END OF ty_zcat_ordem.

DATA: it_ztparam    TYPE TABLE OF ztparam WITH HEADER LINE,
      it_zcat_ordem TYPE TABLE OF ty_zcat_ordem WITH HEADER LINE.


*IF  'IE02_IE01_IE31' CS sy-tcode
*AND sy-dynnr EQ '1022'
*AND sy-repid EQ 'SAPLXEQM'
*AND data_equi-eqart IS NOT INITIAL.
*
*
*  SELECT *
*  FROM ZTPARAM
*  INTO CORRESPONDING FIELDS OF TABLE IT_ZTPARAM
*  WHERE PARAM EQ 'CAT_EQUIP'.
*
*  SORT IT_ZTPARAM ASCENDING BY ZINDEX.
*  CHECK IT_ZTPARAM[] IS NOT INITIAL.
*  FREE IT_ZCAT_ORDEM[].
*  LOOP AT IT_ZTPARAM.
*    IT_ZCAT_ORDEM-PARAM = IT_ZTPARAM-PARAM.
*    IT_ZCAT_ORDEM-ZVAL  = IT_ZTPARAM-ZVAL.
*    IT_ZCAT_ORDEM-CONST = IT_ZTPARAM-CONST.
*    APPEND IT_ZCAT_ORDEM.
*    CLEAR IT_ZCAT_ORDEM.
*    CLEAR IT_ZTPARAM.
*  ENDLOOP.
*
*  READ TABLE IT_ZCAT_ORDEM WITH KEY ZVAL = DATA_EQUI-EQTYP.
*  IF SY-SUBRC = 0.
*    DATA: "TL_TEMP TYPE ZPMR0001 with header line,
*      BEGIN OF TL_TEMP OCCURS 0,
*        EQART TYPE ZPMR0001-CLASS_OPER,
*        HERST TYPE ZPMR0001-HERST,
*        TYPBZ TYPE ZPMR0001-TYPBZ,
*        GROES TYPE ZPMR0001-GROES,
*      END OF TL_TEMP,
*      LV_ERRO   TYPE I,
*
*      LV_PROG   TYPE SY-REPID,
*      LV_TELA   TYPE SY-DYNNR,
*      LV_HERST  TYPE DFIES-FIELDNAME,
*      LV_FIELD  TYPE HELP_INFO-DYNPROFLD,
*      LV_TEMP   TYPE C,
*
*      TL_DFIES  TYPE TABLE OF DFIES      WITH HEADER LINE,
*      TL_RETURN TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
*      TL_DSELC  TYPE TABLE OF DSELC      WITH HEADER LINE.
*
*    SELECT DISTINCT CLASS_OPER HERST TYPBZ GROES
*      INTO TABLE TL_TEMP
*      FROM ZPMR0001
*     WHERE CLASS_OPER EQ DATA_EQUI-EQART.
*
*    IF SY-SUBRC IS INITIAL.
*      READ TABLE TL_TEMP WITH KEY HERST = DATA_EQUI-HERST.
*      IF SY-SUBRC IS NOT INITIAL.
*        ADD 1 TO LV_ERRO.
*        MESSAGE 'Fabricante não encontrado.' TYPE'S' DISPLAY LIKE 'E'.
*        CLEAR DATA_EQUI-HERST.
*      ELSE.
*        DELETE TL_TEMP WHERE HERST NE DATA_EQUI-HERST.
*      ENDIF.
*
*      IF LV_ERRO IS INITIAL.
*        READ TABLE TL_TEMP WITH KEY TYPBZ = DATA_EQUI-TYPBZ.
*        IF SY-SUBRC IS NOT INITIAL.
*          ADD 1 TO LV_ERRO.
*          MESSAGE 'Modelo não encontrado para este fabricante.' TYPE 'S' DISPLAY LIKE 'E'.
*          CLEAR DATA_EQUI-TYPBZ.
*        ELSE.
*          DELETE TL_TEMP WHERE TYPBZ NE DATA_EQUI-TYPBZ.
*        ENDIF.
*      ENDIF.
*
*      IF  DATA_EQUI-EQTYP EQ 'T'
*      AND LV_ERRO IS INITIAL.
*        READ TABLE TL_TEMP WITH KEY GROES = DATA_EQUI-GROES.
*        IF SY-SUBRC IS NOT INITIAL.
*          ADD 1 TO LV_ERRO.
*          MESSAGE 'Tamanho especificado incorreto.' TYPE 'S' DISPLAY LIKE 'E'.
*          CLEAR DATA_EQUI-GROES.
*        ENDIF.
*      ENDIF.
*
*    ELSE.
*      ADD 1 TO LV_ERRO.
*      MESSAGE 'O tipo de objeto informado não possui modelos atribuido.' TYPE 'S' DISPLAY LIKE 'E'.
*      CLEAR DATA_EQUI-EQART.
*      EXIT.
*
*    ENDIF.
*
*    IF  LV_ERRO IS NOT INITIAL
*    AND TL_TEMP[] IS NOT INITIAL.
*      CLEAR: DATA_EQUI-HERST,
*             DATA_EQUI-TYPBZ,
*             DATA_IFLO-GROES.
*
*      LV_PROG = SY-REPID.
*      LV_TELA = SY-DYNNR.
*      LV_HERST = 'TYPBZ'.
**      LV_FIELD = 'ITOB-HERST'.
*
*      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*        EXPORTING
*          RETFIELD        = LV_HERST
*          DYNPPROG        = LV_PROG
*          DYNPNR          = LV_TELA
**         DYNPROFIELD     = LV_FIELD
*          VALUE_ORG       = 'S'
*        IMPORTING
*          USER_RESET      = LV_TEMP
*        TABLES
*          VALUE_TAB       = TL_TEMP
*          RETURN_TAB      = TL_RETURN
*          DYNPFLD_MAPPING = TL_DSELC
*          FIELD_TAB       = TL_DFIES.
*
*      IF SY-SUBRC IS INITIAL.
*        READ TABLE TL_TEMP WITH KEY TYPBZ = TL_RETURN-FIELDVAL.
*        IF SY-SUBRC IS INITIAL.
*          DATA_EQUI-HERST = TL_TEMP-HERST.
*          DATA_EQUI-TYPBZ = TL_TEMP-TYPBZ.
*          DATA_EQUI-GROES = TL_TEMP-GROES.
**        DATA_EQKT-GROES = TL_TEMP-GROES.
**        DATA_EQUZ-GROES = TL_TEMP-GROES.
**        DATA_ILOA-GROES = TL_TEMP-GROES.
*          DATA_IFLO-GROES = TL_TEMP-GROES.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.
*ENDIF.
IF  'IE02_IE01_IE31' CS sy-tcode.
*  "Automatizar descrição do veiculo.
*  TRY .
*      IF data_equi-eqtyp EQ 'V' OR data_equi-eqtyp EQ 'A' OR data_equi-eqtyp EQ 'T'.
*        zcl_pm_data_equipament=>zif_pm_data_equipament~get_instance(
*        )->set_eartx( i_eqart =  data_equi-eqart
*        )->set_desc_fabricante( i_herst =  data_equi-herst
*        )->set_desc_modelo( i_typbz =  data_equi-typbz
*        )->set_desc_eqpto( IMPORTING e_ktx01 = data_eqkt-eqktx ).
*
*        IF data_eqkt-eqktx IS NOT INITIAL.
*
*        ENDIF.
*      ENDIF.
*    CATCH zcx_error INTO DATA(ex_erro).
*
*  ENDTRY.
ENDIF.
