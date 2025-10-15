*"             VALUE(MASTER_EQUIPMENT) LIKE  V_EQUI STRUCTURE  V_EQUI
*"             VALUE(SUBEQUIPMENT)     LIKE  V_EQUI STRUCTURE  V_EQUI
*&---------------------------------------------------------------------*
*&  Include           ZXEQMU01
*&---------------------------------------------------------------------*

IF SY-TCODE = 'IE01'
OR SY-TCODE = 'IE02'
OR SY-TCODE = 'IE31'.
** Checa se é um pneu
  CHECK SUBEQUIPMENT-EQTYP EQ 'T'.

  TYPES: BEGIN OF TY_POSICOES,
          CONST TYPE ZTPARAM-CONST,
          ZVAL  TYPE ZTPARAM-ZVAL,
         END OF TY_POSICOES,

         BEGIN OF TY_EQUI,
           EQUNR     TYPE EQUZ-EQUNR,      " Nº Equipamento
           HEQUI     TYPE EQUZ-HEQUI,      " Equipamento Superior
           HEQNR     TYPE EQUZ-HEQNR,      " Item
           IWERK     TYPE EQUZ-IWERK,      " Centro Planejamento
           TIDNR     TYPE EQUZ-TIDNR,      " Fogo
           GEWRK     TYPE EQUZ-GEWRK,      " Centro de trabalho
           EQTYP     TYPE EQUI-EQTYP,      " Ctg Equipamento
           EQART     TYPE EQUI-EQART,      " Tipo do objeto técnico
           HERST     TYPE EQUI-HERST,      " Fabricante
           TYPBZ     TYPE EQUI-TYPBZ,      " Denominação do tipo
           OBJNR     TYPE EQUI-OBJNR,      " Objeto
           DATBI     TYPE EQUZ-DATBI,      " Última data de validade de montagem
           TIMBI     TYPE EQUZ-TIMBI,      " Última hora de montagem
           NUM_AXLE  TYPE FLEET-NUM_AXLE,  " Número de eixos
         END OF TY_EQUI.

  DATA: TL_PARAM    TYPE TABLE OF ZTPARAM WITH HEADER LINE,
        TL_EQUIP    TYPE TABLE OF TY_EQUI WITH HEADER LINE,
        TL_RESULT   TYPE TABLE OF STRING,
        WL_RESULT   TYPE STRING,
        TL_POSICOES TYPE TABLE OF TY_POSICOES,
        WL_POSICOES TYPE TY_POSICOES,
        LV_CONT_EQP TYPE I,
        LV_NUM_EIXO TYPE I,
        LV_MSG      TYPE C LENGTH 255.

** Buscar dados de posição de pneus
  SELECT CONST ZVAL
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE TL_PARAM
    WHERE PARAM = 'NUM_ITEM'.

  IF SY-SUBRC = 0.
    LOOP AT TL_PARAM.
      SPLIT TL_PARAM-ZVAL AT ';' INTO TABLE TL_RESULT.
      IF SY-SUBRC IS INITIAL.
        LOOP AT TL_RESULT INTO WL_RESULT.
          WL_POSICOES-CONST = TL_PARAM-CONST.
          WL_POSICOES-ZVAL  = WL_RESULT.
          APPEND WL_POSICOES TO TL_POSICOES.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    IF TL_POSICOES IS INITIAL.
      MESSAGE 'Não há posições configuradas na ZPM0002.' TYPE 'I'.
      RAISE HIERARCHY_NOT_ALLOWED.
    ELSE.
** Valida se a posição informado foi paramentrizada na ZPM0002
      READ TABLE TL_POSICOES INTO WL_POSICOES WITH KEY ZVAL = SUBEQUI_POSITION.
      IF SY-SUBRC IS INITIAL.
** Pega número de eixos equipamento superior
        SELECT *
          FROM EQUZ
          INNER JOIN EQUI ON EQUI~EQUNR = EQUZ~EQUNR
          INNER JOIN FLEET ON FLEET~OBJNR = EQUI~OBJNR
          INTO CORRESPONDING FIELDS OF TABLE TL_EQUIP
          WHERE EQUZ~EQUNR EQ MASTER_EQUIPMENT-EQUNR
           AND  DATBI      GT SUBEQUIPMENT-AEDAT.

        SORT TL_EQUIP BY DATBI DESCENDING TIMBI DESCENDING.
        READ TABLE TL_EQUIP INDEX 1.
        LV_NUM_EIXO = TL_EQUIP-NUM_AXLE.
        MULTIPLY LV_NUM_EIXO BY 4.

** Checa demais pneus montados no equipamento superior informado
        SELECT *
          FROM EQUZ
          INNER JOIN EQUI ON EQUI~EQUNR = EQUZ~EQUNR
          INTO CORRESPONDING FIELDS OF TABLE TL_EQUIP
          WHERE HEQUI EQ MASTER_EQUIPMENT-EQUNR
           AND  DATBI GT SUBEQUIPMENT-AEDAT
           AND  EQTYP EQ 'T'.

        DESCRIBE TABLE TL_EQUIP LINES LV_CONT_EQP.
        ADD 1 TO LV_CONT_EQP.

** Valida numero de pneus pelo número de eixos
        IF LV_CONT_EQP GT LV_NUM_EIXO.
          MESSAGE 'Número de pneus excedido.' TYPE 'I'.
          RAISE HIERARCHY_NOT_ALLOWED.
        ENDIF.

** Valida se já existe equipamento nesta posição
        LOOP AT TL_POSICOES INTO WL_POSICOES WHERE CONST  = WL_POSICOES-CONST.
          READ TABLE TL_EQUIP WITH KEY HEQNR = WL_POSICOES-ZVAL.
          IF SY-SUBRC IS INITIAL.
            READ TABLE TL_PARAM WITH KEY CONST = WL_POSICOES-CONST.
            CONCATENATE 'Já existe um equipamento montado nesta posição (' TL_PARAM-ZVAL ').' INTO LV_MSG.
            MESSAGE LV_MSG TYPE 'I'.
            RAISE HIERARCHY_NOT_ALLOWED.
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE 'Posição de montagem inválida.' TYPE 'I'.
        RAISE HIERARCHY_NOT_ALLOWED.
      ENDIF.
    ENDIF.

  ENDIF.

ENDIF.
