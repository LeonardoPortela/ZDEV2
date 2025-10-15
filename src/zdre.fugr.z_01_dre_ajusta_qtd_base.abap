FUNCTION Z_01_DRE_AJUSTA_QTD_BASE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PBUKRS) TYPE  PBUKRS
*"----------------------------------------------------------------------

  DATA: IT_DRE_04  TYPE TABLE OF ZGLT_DRE_04 WITH HEADER LINE,
        LC_UNIDADE TYPE MEINS,
        LC_QUANTID TYPE BSTMG,
        LC_MATNR   TYPE MATNR,
        I_MENGE    LIKE EKPO-MENGE,
        E_MENGE    LIKE EKPO-MENGE.

  FIELD-SYMBOLS: <FS_DRE_04> TYPE ZGLT_DRE_04.

  SELECT * INTO TABLE IT_DRE_04
    FROM ZGLT_DRE_04
   WHERE RUNIT NE SPACE
     AND BUKRS EQ PBUKRS.

  LOOP AT IT_DRE_04 ASSIGNING <FS_DRE_04>.

    LC_MATNR   = <FS_DRE_04>-MATNR.
    LC_UNIDADE = <FS_DRE_04>-RUNIT.
    LC_QUANTID = <FS_DRE_04>-QTMSL.

    IF LC_UNIDADE IS NOT INITIAL.

      CASE LC_UNIDADE.
        WHEN 'UN'. "Unidade
          <FS_DRE_04>-RUNIT_BASE = LC_UNIDADE.
          <FS_DRE_04>-QTMSL_BASE = 0.
        WHEN 'KG' OR 'L' OR 'MWH'. "Quilograma/Litro/Megawatt hora/Microampère
          <FS_DRE_04>-RUNIT_BASE = LC_UNIDADE.
          <FS_DRE_04>-QTMSL_BASE = <FS_DRE_04>-QTMSL.
        WHEN 'G' OR 'TON' OR 'TO' OR 'BAG' OR 'SC'. "Outras Unidades de Medida -- Grama/Toneladas EUA/Tonelada/Saco/Tambor

          <FS_DRE_04>-RUNIT_BASE = 'KG'. "Quilograma

          MOVE LC_QUANTID TO I_MENGE.

          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              I_MATNR              = LC_MATNR
              I_IN_ME              = LC_UNIDADE
              I_OUT_ME             = 'KG'
              I_MENGE              = I_MENGE
            IMPORTING
              E_MENGE              = E_MENGE
            EXCEPTIONS
              ERROR_IN_APPLICATION = 1
              ERROR                = 2
              OTHERS               = 3.

          IF SY-SUBRC IS NOT INITIAL.
            CLEAR: <FS_DRE_04>-RUNIT_BASE, <FS_DRE_04>-QTMSL_BASE.
          ELSE.
            MOVE E_MENGE TO <FS_DRE_04>-QTMSL_BASE.
          ENDIF.

        WHEN OTHERS.
          <FS_DRE_04>-RUNIT_BASE = LC_UNIDADE.
          <FS_DRE_04>-QTMSL_BASE = <FS_DRE_04>-QTMSL.
      ENDCASE.

    ELSE.
      CLEAR: <FS_DRE_04>-RUNIT_BASE.
      <FS_DRE_04>-QTMSL_BASE = 0.
    ENDIF.

  ENDLOOP.

  IF IT_DRE_04[] IS NOT INITIAL.
    MODIFY ZGLT_DRE_04 FROM TABLE IT_DRE_04.
  ENDIF.

ENDFUNCTION.
