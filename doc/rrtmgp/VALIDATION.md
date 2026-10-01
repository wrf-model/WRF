# WRF RRTMGP 37 실행 검증

2026년 10월 1일, GNU Fortran 13과 로컬 NetCDF C 4.9.2 및 Fortran 4.5.4에서 검증했다. 이 결과는 개발 이식본의 CPU 실행 및 출력 계약을 확인한다. 예보 정확도, 다른 컴파일러, MPI/OpenMP 및 GPU 검증은 포함하지 않는다.

## 완료한 확인

| 확인 | 결과 |
| --- | --- |
| 원래 RTE LW 해법, SW 해법, 광학 연산 단위시험 3종 | 모두 통과 |
| vendored 라이브러리 Make 및 독립 CMake 빌드 | 통과 |
| WRF 어댑터 컬럼 시험 | 통과 |
| 같은 컬럼 시험의 배열 경계 및 invalid/zero/overflow 부동소수점 검사 | 통과 |
| WRF em_scm_xy GNU serial 전체 빌드 | wrf.exe와 ideal.exe 생성 |
| 최종 선택 인자 수정 | LW/SW 래퍼 재컴파일, WRF/ideal 재링크 |
| LW/SW 37번, 시간 간격 10초, 5분 적분 | SUCCESS COMPLETE WRF |
| 같은 사례의 기존 LW/SW 4번 | SUCCESS COMPLETE WRF |
| 37번 동일 조건 재실행 | 확인한 복사·온도·바람 필드가 비트 단위로 일치 |
| aer_opt=1 및 cldovrlp=4 설정 | RRTMGP 설정 검사에서 명시적으로 거부 |
| git diff --check | 통과 |

컬럼 시험은 맑은 하늘, 액체 전운량, 부분 구름과 액체·빙정·눈이 포함된 장면, 중첩 옵션 0~3 및 야간을 다룬다. 플럭스와 가열률의 에너지 일관성, clear sky 보존, 직달·산란 및 가시광·근적외 합계, 표본 시드 재현성을 검사한다.

WRF 시험은 1999년 10월 22일 19:00부터 19:05 UTC까지 수행했다. 역사 출력 6개 시각에서 지면·상단 플럭스, 누적 에너지 및 복사 경향이 유한함을 확인했다. `SWDOWN=SWDDIR+SWDDIF`, `RTHRATEN=RTHRATLW+RTHRATSW`도 확인했다. 반복 일치 검사는 SWDOWN, GLW, SWDDIR, SWDDIF, 세 복사 경향, ACSWDNB, ACLWDNB, T 및 W를 대상으로 했다.

37번의 이 사례 최대 SWDOWN은 284.65 W/m², 최대 GLW는 281.03 W/m²였다. 같은 조건의 4번은 각각 230.89 및 282.61 W/m²였다. 서로 다른 계수, 구름 광학 및 표본화의 결과이므로 이러한 수치 차이를 정확도 개선으로 해석하지 않는다. 4번 실행 성공은 기존 경로가 작동함을 확인하며, 수정 전 WRF와의 비트 단위 회귀 비교는 수행하지 않았다.

## 발견한 문제와 시험 조건

초기 전체 빌드에서 WRF의 오류 함수가 모듈 내부가 아닌 외부 함수라는 차이를 수정했다. 실제 WRF 실행에서는 비화학 빌드가 전달하지 않는 선택 인자 `aer_ra_feedback`를 새 분기에서 접근해 메모리 오류가 발생했다. LW/SW 모두 `PRESENT` 확인을 추가했고 실제 실행으로 재검증했다. 단독 컬럼 시험만으로는 WRF 선택 인자 연결 오류를 발견할 수 없었다.

원본 SCM의 60초 시간 간격으로 37번을 실행하면 작은 3×3 주기 격자에서 CFL 초과 후 온도 범위를 벗어나 종료됐다. 10초로 줄인 동일 5분 사례는 정상 완료했고 반복 결과가 일치했다. 서로 다른 컬럼의 McICA 시드가 수평 차이를 만들기 때문에 원본 단일 컬럼의 균일성 가정과 관계가 있을 수 있으나, 이 원인 해석은 추가 검증이 필요하다. 재현용 namelist는 시간 간격 10초를 명시한다.

미지원 설정 시험은 종료 코드만으로 판단하지 않았다. 이 WRF 실행에서 설정 오류가 STOP 메시지와 함께 종료 코드 0으로 끝나는 경우가 있어 로그의 RRTMGP 오류 메시지를 검사했다. `run_scm.sh`는 정상 완료 메시지를 확인한다.

## 재현과 근거 파일

상위 작업 디렉터리에서 다음을 실행한다. NumPy와 Python netCDF4가 필요하다.

```bash
LD_LIBRARY_PATH="$NETCDF/lib:${LD_LIBRARY_PATH:-}" \
  WRF/test/rrtmgp/run_scm.sh build/new-scm37 37
LD_LIBRARY_PATH="$NETCDF/lib:${LD_LIBRARY_PATH:-}" \
  WRF/test/rrtmgp/run_scm.sh build/new-scm4 4
python3 WRF/test/rrtmgp/validate_scm.py build/new-scm37 build/new-scm4
```

이 작업 공간의 근거는 `research/validation/`에 있다: RTE 단위시험 로그 3개, `rrtmgp-ctest.log`, `rrtmgp-ctest-debug.log`, `scm-final.json`, `scm-extra-checks.json`. 전체 빌드 로그는 `build/wrf-compile-scm-final.log`, 마지막 래퍼 컴파일 로그는 `build/wrf-wrapper-fix.log`다. 정상 실행 디렉터리는 `build/scm37-dt10`, `build/scm4-dt10`, `build/scm37-repeat`이다.

전체 WRF CMake 빌드는 수행하지 않았다. CMake 검증 범위는 vendored 라이브러리와 독립 어댑터 시험이다. 실제 예보 도메인, 지형, 중첩, SSiB, 화학 결합, 계수 범위 밖 대기 상태 및 병렬 성능은 아직 검증하지 않았다. 장파 산란과 에어로졸을 포함한 HAFS 수준의 전체 suite 이식은 후속 작업이다.
