"Name: \FU:HELP_START\SE:BEGIN\EI
ENHANCEMENT 0 ZPM_PREENCHE_MATCHCODE.

if sy-tcode = 'IE01' or
   sy-tcode = 'IE02' or
   sy-tcode = 'IE03'.
*   or sy-tcode = 'ZREGISTER_DATA'.

  if HELP_INFOS-DYNPROFLD = 'ITOB-HERST'.
    HELP_INFOS-call = 'M'.
    HELP_INFOS-mcobj = 'ZPMFABRICANTE'.

    DYNPSELECT-dyfldname = 'sy-mandt'.
    DYNPSELECT-fldname   = 'MANDT'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = 'ITOB-EQART'.
    DYNPSELECT-fldname   = 'CLASS_OPER'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = 'ITOB-HERST'.
    DYNPSELECT-fldname   = 'HERST'.
    append DYNPSELECT.

  elseif HELP_INFOS-DYNPROFLD = '<FS_WA_REGISTRO_MANTER>-HERST'.
    HELP_INFOS-call = 'M'.
    HELP_INFOS-mcobj = 'ZPMFABRICANTE'.

    DYNPSELECT-dyfldname = 'sy-mandt'.
    DYNPSELECT-fldname   = 'MANDT'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = 'ITOB-EQART'.
    DYNPSELECT-fldname   = 'CLASS_OPER'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = '<FS_WA_REGISTRO_MANTER>-HERST'.
    DYNPSELECT-fldname   = 'HERST'.
    append DYNPSELECT.

  elseif HELP_INFOS-DYNPROFLD = 'ITOB-TYPBZ'.
    HELP_INFOS-call = 'T'.
    HELP_INFOS-checktable = 'ZPMR0001'.
    HELP_INFOS-checkfield = 'TYPBZ'.

    DYNPSELECT-dyfldname = 'sy-mandt'.
    DYNPSELECT-fldname   = 'MANDT'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = 'ITOB-EQART'.
    DYNPSELECT-fldname   = 'CLASS_OPER'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = 'ITOB-HERST'.
    DYNPSELECT-fldname   = 'HERST'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = 'ITOB-TYPBZ'.
    DYNPSELECT-fldname   = 'TYPBZ'.
    append DYNPSELECT.

  elseif HELP_INFOS-DYNPROFLD = '<FS_WA_REGISTRO_MANTER>-TYPBZ'.
    HELP_INFOS-call = 'T'.
    HELP_INFOS-checktable = 'ZPMR0001'.
    HELP_INFOS-checkfield = 'TYPBZ'.

    DYNPSELECT-dyfldname = 'sy-mandt'.
    DYNPSELECT-fldname   = 'MANDT'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = 'ITOB-EQART'.
    DYNPSELECT-fldname   = 'CLASS_OPER'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = '<FS_WA_REGISTRO_MANTER>-HERST'.
    DYNPSELECT-fldname   = 'HERST'.
    append DYNPSELECT.

    DYNPSELECT-dyfldname = '<FS_WA_REGISTRO_MANTER>-TYPBZ'.
    DYNPSELECT-fldname   = 'TYPBZ'.
    append DYNPSELECT.

  endif.


endif.

*if sy-tcode eq 'ZREGISTER_DATA'.
*  if HELP_INFOS-DYNPROFLD = '<FS_WA_REGISTRO_MANTER>-RAZAO_ESPECIAL'
*    AND HELP_INFOS-call = 'M'
*    AND HELP_INFOS-mcobj = 'ZAJD_T074U'.
*
*    IF HELP_INFOS-FLDVALUE EQ '='.
*    HELP_INFOS-FLDVALUE = '=='.
*    ENDIF.
*  endif.
*endif.


ENDENHANCEMENT.
