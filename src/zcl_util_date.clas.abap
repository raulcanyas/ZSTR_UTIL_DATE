class ZCL_UTIL_DATE definition
  public
  final
  create public .

public section.

  types:
    tty_day_name   TYPE TABLE OF t246 .
  types:
    tty_month_name TYPE TABLE OF t247 .

  constants:
    BEGIN OF cs_mask,
        ddmmyyyy TYPE char8 VALUE 'DDMMYYYY',
        ddmmyy   TYPE char8 VALUE 'DDMMYY',
        mmddyyyy TYPE char8 VALUE 'MMDDYYYY',
        mmddyy   TYPE char8 VALUE 'MMDDYY',
        yyyymmdd TYPE char8 VALUE 'YYYYMMDD',
        yymmdd   TYPE char8 VALUE 'YYMMDD',
      END OF cs_mask .

  class-methods ADD_MONTH_2_DATE
    importing
      !IV_NUM_MONTH type INT4
      !IV_IN_DATE type SYDATUM default SY-DATUM
    returning
      value(RV_OUT_DATE) type SYDATUM .
  class-methods DIF_MONTH_2_DATE
    importing
      !IV_NUM_MONTH type INT4
      !IV_IN_DATE type SYDATUM default SY-DATUM
    returning
      value(RV_OUT_DATE) type SYDATUM .
  class-methods MONTH_NAMES
    importing
      !IV_LANGU type LANGU default SY-LANGU
    exporting
      !ET_NAMES type TTY_MONTH_NAME .
  class-methods DAY_NAMES
    importing
      !IV_LANGU type LANGU default SY-LANGU
    exporting
      !ET_NAMES type TTY_DAY_NAME .
  class-methods WEEK_DATE
    importing
      !IV_DATE type DATUM default SY-DATUM
    returning
      value(RV_NUM_WEEK) type INT4 .
  class-methods DAY_IN_YEAR
    importing
      !IV_DATE type DATUM default SY-DATUM
    returning
      value(RV_NUM_DAY) type INT4 .
  class-methods DAY_IN_WEEK
    importing
      !IV_DATE type DATUM default SY-DATUM
    returning
      value(RV_NUM_DAY) type INT4 .
  class-methods IS_WEEKEND
    importing
      !IV_DATE type DATUM default SY-DATUM
    returning
      value(RV_IS_WEEKEND) type FLAG .
  class-methods TEXT_DATE
    importing
      !IV_DATE type DATUM default SY-DATUM
      !IV_LANGU type LANGU default SY-LANGU
    exporting
      !EV_SHORT type TEXT50
      !EV_LONG type TEXT50 .
  class-methods FORMAT_DATE
    importing
      !IV_DATE type DATUM default SY-DATUM
      !IV_MASK type CHAR8 default 'DDMMYYYY'
      !IV_SEP type CHAR1 default '.'
    returning
      value(RV_DATE) type CHAR10 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UTIL_DATE IMPLEMENTATION.


METHOD ADD_MONTH_2_DATE.

    DATA: BEGIN OF dat,
            jjjj(4) ,
            mm(2) ,
            tt(2) ,
          END OF dat,

          BEGIN OF hdat,
            jjjj(4) ,
            mm(2) ,
            tt(2) ,
          END OF hdat,
          newmm    TYPE p,
          diffjjjj TYPE p.

    WRITE:  iv_in_date+0(4) TO dat-jjjj,
            iv_in_date+4(2) TO  dat-mm,
            iv_in_date+6(2) TO  dat-tt.
    diffjjjj =   ( dat-mm + iv_num_month - 1 ) DIV 12.
    newmm    =   ( dat-mm + iv_num_month - 1 ) MOD 12 + 1.
    dat-jjjj = dat-jjjj +  diffjjjj.

    IF newmm < 10.
      WRITE '0' TO  dat-mm+0(1).
      WRITE newmm TO  dat-mm+1(1).
    ELSE.
      WRITE newmm TO  dat-mm.
    ENDIF.
    IF dat-tt > '28'.
      hdat-tt = '01'.
      newmm   = ( dat-mm  )  MOD 12 + 1.
      hdat-jjjj = dat-jjjj + ( (  dat-mm ) DIV 12 ).

      IF newmm < 10.
        WRITE '0' TO hdat-mm+0(1).
        WRITE newmm TO hdat-mm+1(1).
      ELSE.
        WRITE newmm TO hdat-mm.
      ENDIF.

      IF dat-tt = '31'.
        rv_out_date = hdat.
        rv_out_date = rv_out_date - 1.
      ELSE.
        IF dat-mm = '02'.
          rv_out_date = hdat.
          rv_out_date = rv_out_date - 1.
        ELSE.
          rv_out_date = dat.
        ENDIF.
      ENDIF.
    ELSE.
      rv_out_date = dat.
    ENDIF.

  ENDMETHOD.


METHOD DAY_IN_WEEK.

    DATA: lv_day TYPE cind.

    CALL FUNCTION 'DATE_COMPUTE_DAY'
      EXPORTING
        date = iv_date
      IMPORTING
        day  = lv_day.

    rv_num_day = lv_day.

  ENDMETHOD.


METHOD DAY_IN_YEAR.

    DATA: lv_last_year TYPE n LENGTH 4,
          lv_end_year  TYPE datum.

    lv_last_year = iv_date+0(4).
    lv_last_year = lv_last_year - 1.

    CONCATENATE lv_last_year '1231' INTO lv_end_year.

    rv_num_day = iv_date - lv_end_year.

  ENDMETHOD.


METHOD DAY_NAMES.

    CALL FUNCTION 'WEEKDAY_GET'
      EXPORTING
        language          = iv_langu
      TABLES
        weekday           = et_names
      EXCEPTIONS
        weekday_not_found = 1
        OTHERS            = 2.

  ENDMETHOD.


METHOD DIF_MONTH_2_DATE.

    DATA: BEGIN OF dat,
            jjjj(4) ,
            mm(2) ,
            tt(2) ,
          END OF dat,

          BEGIN OF hdat,
            jjjj(4) ,
            mm(2) ,
            tt(2) ,
          END OF hdat,
          newmm    TYPE p,
          diffjjjj TYPE p.

    WRITE:  iv_in_date+0(4) TO dat-jjjj,
            iv_in_date+4(2) TO  dat-mm,
            iv_in_date+6(2) TO  dat-tt.

    diffjjjj =   ( dat-mm - iv_num_month - 1 ) DIV 12.
    newmm    =   ( dat-mm - iv_num_month - 1 ) MOD 12 + 1.
    dat-jjjj = dat-jjjj +  diffjjjj.

    IF newmm < 10.
      WRITE '0' TO  dat-mm+0(1).
      WRITE newmm TO  dat-mm+1(1).
    ELSE.
      WRITE newmm TO  dat-mm.
    ENDIF.
    IF dat-tt > '28'.
      hdat-tt = '01'.
      newmm   = ( dat-mm  )  MOD 12 + 1.
      hdat-jjjj = dat-jjjj + ( (  dat-mm ) DIV 12 ).

      IF newmm < 10.
        WRITE '0' TO hdat-mm+0(1).
        WRITE newmm TO hdat-mm+1(1).
      ELSE.
        WRITE newmm TO hdat-mm.
      ENDIF.

      IF dat-tt = '31'.
        rv_out_date = hdat.
        rv_out_date = rv_out_date - 1.
      ELSE.
        IF dat-mm = '02'.
          rv_out_date = hdat.
          rv_out_date = rv_out_date - 1.
        ELSE.
          rv_out_date = dat.
        ENDIF.
      ENDIF.
    ELSE.
      rv_out_date = dat.
    ENDIF.

  ENDMETHOD.


METHOD FORMAT_DATE.

    CASE iv_mask.
      WHEN cs_mask-ddmmyyyy.
        IF iv_sep IS NOT INITIAL.
          CONCATENATE iv_date+6(2) iv_sep
                      iv_date+4(2) iv_sep
                      iv_date+0(4) INTO rv_date.
        ELSE.
          CONCATENATE iv_date+6(2)
                      iv_date+4(2)
                      iv_date+0(4) INTO rv_date.
        ENDIF.

      WHEN cs_mask-ddmmyy.
        IF iv_sep IS NOT INITIAL.
          CONCATENATE iv_date+6(2) iv_sep
                      iv_date+4(2) iv_sep
                      iv_date+2(2) INTO rv_date.
        ELSE.
          CONCATENATE iv_date+6(2)
                      iv_date+4(2)
                      iv_date+2(2) INTO rv_date.
        ENDIF.

      WHEN cs_mask-mmddyyyy.
        IF iv_sep IS NOT INITIAL.
          CONCATENATE iv_date+4(2) iv_sep
                      iv_date+6(2) iv_sep
                      iv_date+0(4) INTO rv_date.
        ELSE.
          CONCATENATE iv_date+4(2)
                      iv_date+6(2)
                      iv_date+0(4) INTO rv_date.
        ENDIF.

      WHEN cs_mask-mmddyy.
        IF iv_sep IS NOT INITIAL.
          CONCATENATE iv_date+4(2) iv_sep
                      iv_date+6(2) iv_sep
                      iv_date+2(2) INTO rv_date.
        ELSE.
          CONCATENATE iv_date+4(2)
                      iv_date+6(2)
                      iv_date+2(2) INTO rv_date.
        ENDIF.

      WHEN cs_mask-yyyymmdd.
        IF iv_sep IS NOT INITIAL.
          CONCATENATE iv_date+0(4) iv_sep
                      iv_date+4(2) iv_sep
                      iv_date+6(2) INTO rv_date.
        ELSE.
          rv_date = iv_date.
        ENDIF.

      WHEN cs_mask-yymmdd.
        IF iv_sep IS NOT INITIAL.
          CONCATENATE iv_date+2(2) iv_sep
                      iv_date+4(2) iv_sep
                      iv_date+6(2) INTO rv_date.
        ELSE.
          CONCATENATE iv_date+2(2)
                      iv_date+4(2)
                      iv_date+6(2) INTO rv_date.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


METHOD IS_WEEKEND.

    DATA: lv_day TYPE int4.

    lv_day = day_in_week( ).

    IF lv_day EQ 6 OR
       lv_day EQ 7.
      rv_is_weekend = 'X'.
    ELSE.
      CLEAR: rv_is_weekend.
    ENDIF.

  ENDMETHOD.


METHOD MONTH_NAMES.

    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language              = iv_langu
      TABLES
        month_names           = et_names
      EXCEPTIONS
        month_names_not_found = 1
        OTHERS                = 2.

  ENDMETHOD.


METHOD TEXT_DATE.

    DATA: lt_month_name TYPE tty_month_name,
          lt_day_name   TYPE tty_day_name,
          ls_month_name TYPE t247,
          ls_day_name   TYPE t246,
          lv_month      TYPE n LENGTH 2,
          lv_day        TYPE int4.

    lv_month = iv_date+4(2).

    " Nº de día de la semana
    lv_day = day_in_week( iv_date ).

    " Nombres de días
    day_names( EXPORTING iv_langu = iv_langu
               IMPORTING et_names = lt_day_name ).

    READ TABLE lt_day_name INTO ls_day_name WITH KEY wotnr = lv_day.
    IF sy-subrc EQ 0.
      ev_short = ls_day_name-kurzt.
      ev_long  = ls_day_name-langt.
    ENDIF.



    " Nombres de meses
    month_names(
      EXPORTING iv_langu = iv_langu
      IMPORTING et_names = lt_month_name ).

    READ TABLE lt_month_name INTO ls_month_name WITH KEY mnr = lv_month.
    IF sy-subrc EQ 0.

      CONCATENATE ev_short iv_date+6(2) ls_month_name-ktx iv_date+0(4) INTO ev_short SEPARATED BY space.
      CONCATENATE ev_long iv_date+6(2) ls_month_name-ltx iv_date+0(4) INTO ev_long  SEPARATED BY space.

    ENDIF.

  ENDMETHOD.


METHOD week_date.

    DATA: lv_week TYPE kweek.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = iv_date
      IMPORTING
        week         = lv_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.

    rv_num_week = lv_week+4(2).

  ENDMETHOD.
ENDCLASS.
