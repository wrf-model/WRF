# WRF RRTMGP radiation option 37

WRF v4.8.0에 RTE+RRTMGP CPU 복사 계산을 장파 및 단파 옵션 37로 연결한 개발 이식본이다. WRF의 기존 RRTMG 전처리로 압력, 온도, 기체 체적혼합비, 구름 분율과 수분 경로를 만들고 `phys/module_ra_rrtmgp.F`에서 RRTMGP 기체 광학과 RTE 해법을 호출한다. 계산한 플럭스와 가열률은 기존 WRF 진단 및 온위 경향 배열로 전달된다.

## 사용 설정

```fortran
&physics
 ra_lw_physics = 37,
 ra_sw_physics = 37,
 aer_opt = 0,
 cldovrlp = 2,
 rrtmgp_data_path = '.',
/
```

`rrtmgp_data_path`는 모든 도메인이 공유하는 계수 디렉터리다. `run/`에 다음 파일이 포함되어 있다. 실행 디렉터리로 복사하거나 이 디렉터리의 경로를 지정한다. 기존 WRF 복사 전처리에 필요한 `RRTMG_LW_DATA`, `RRTMG_SW_DATA`, 오존 및 온실기체 자료도 기존 방식으로 배치한다.

- `rrtmgp-gas-lw-g128.nc`
- `rrtmgp-gas-sw-g112.nc`
- `rrtmgp-clouds-lw-bnd.nc`
- `rrtmgp-clouds-sw-bnd.nc`

## 코드와 자료 버전

WRF 기준은 태그 v4.8.0, 커밋 `06d4240ae989cc3e50af412bb472df3d9048783c`이다. 이식 브랜치는 `rrtmgp-37`이다.

`external/rte_rrtmgp/`는 UFS CCPP 커밋 `3e6660c6df54e95a0871e990c2294dd397ae3860`이 고정한 NCAR RTE+RRTMGP 커밋 `41c5fcd950fed09b8afe186dede266824eca7fd3`의 소스다. 최신 upstream과 API를 섞지 않고 실제 UFS 코드 경로에 맞췄다. 출처 및 로컬 변경은 `external/rte_rrtmgp/SOURCE.json`에 기록했다.

계수는 earth-system-radiation/rrtmgp-data 커밋 `ea788bb39876948fa8d2c235665ccff19b4686b5`에서 가져왔다. 파일 URL, 크기 및 SHA256은 `external/rte_rrtmgp/DATA.json`에 있다. 현재 공개 자료의 구름 필드 이름 및 빙정 유효직경 좌표를 읽도록 예제 로더를 수정했다. 자료 라이선스는 `external/rte_rrtmgp/DATA_LICENSE`에 보존한다.

## 구현 범위

CPU double precision 내부 계산, H2O/CO2/O3/N2O/CH4/O2 여섯 기체, LW 128 및 SW 112 g점, 장파 흡수·방출과 단파 2 stream 해법을 사용한다. 장파 산란은 포함하지 않는다. 액체·빙정·눈의 밴드 광학을 구한 뒤 McICA로 g점에 표본화한다. `cldovrlp=0`은 맑은 하늘, 1은 random, 2는 maximum random, 3은 maximum이다. 표본은 수평 격자 위치와 날짜에 따른 재현 가능한 시드로 만든다.

기존 옵션 4는 원래 RRTMG 호출 경로를 사용한다. 37의 all sky 및 clear sky 플럭스, K/day 가열률, 단파 직달·산란과 가시광·근적외 분할을 기존 출력에 연결했다. WRF 래퍼가 K/day를 온위 경향으로 변환한다.

| WRF 입력 | RRTMGP 처리 |
| --- | --- |
| hPa 압력, 지면부터 위로 배열 | Pa로 변환, `top_at_1=.false.` |
| 기체 체적혼합비 | 그대로 전달, 내부 double precision 변환 |
| 구름 안의 수분 경로 g/m² | 구름 LUT에 전달, 분율은 McICA에 별도 적용 |
| 액체 유효반경 µm | LUT 유효반경 범위로 제한 |
| WRF Fu 빙정 크기 | 1.0315 변환을 되돌리고 유효직경으로 변환 |
| 그 밖의 빙정·눈 유효반경 | 두 배로 유효직경 변환, LUT 범위로 제한 |
| 지면 장파 방사율 | 회색 또는 16 밴드 입력 |
| 태양상수 및 천정각 | WRF 계절·일식 보정값 사용 |

SW 밴드 하한 12850 cm⁻¹ 이상을 가시광 출력에, 나머지를 근적외 출력에 누적한다. 이는 계수 밴드 경계에 맞춘 분할이며 정밀한 파장 0.7 µm 절단과 차이가 있다. 에어로졸은 밴드 경계와 순서가 RRTMG와 달라 직접 전달할 수 없다. `aer_opt!=0`, 화학 에어로졸 피드백 및 CMAQ 피드백, `cldovrlp=4,5`를 거부한다. CFC11/12/22 및 CCl4는 이 6 기체 구현에 포함하지 않는다.

이 구현은 HAFS 전체 복사 suite의 재현을 목표로 한 결과가 아니다. HAFS의 최적화된 LW 78/SW 75 g점 및 장파 산란, 에어로졸 경로는 추가 이식 대상이다. GPU, 실제 예보 사례, MPI/OpenMP 확장성 및 관측 비교는 별도 검증이 필요하다.

## 빌드 및 독립 시험

WRF의 기존 Make 빌드와 CMake 소스 목록에 라이브러리를 연결했다. NetCDF C 및 Fortran 개발 파일이 필요하다. 전통적인 빌드는 `NETCDF` 아래 `include/`와 `lib/`를 사용한다. 본 환경에서는 로컬 추출 의존성으로 빌드하며 시스템 패키지는 설치하지 않았다.

독립 컬럼 시험은 WRF 전체 빌드 없이 다음과 같이 실행한다.

```bash
cmake -S WRF/test/rrtmgp -B build/rrtmgp-test \
  -DNETCDF_INCLUDE_DIR="$NETCDF/include" \
  -DNETCDF_LIBRARY_DIR="$NETCDF/lib"
cmake --build build/rrtmgp-test -j 8
LD_LIBRARY_PATH="$NETCDF/lib:${LD_LIBRARY_PATH:-}" \
  ctest --test-dir build/rrtmgp-test --output-on-failure
```

이 명령은 상위 작업 디렉터리에서 실행한다. 시험은 맑은 하늘 all/clear 일치, 흐린 하늘의 clear sky 보존, 구름에 의한 지면 단파 감소, 플럭스와 가열률의 에너지 일관성, 직달·산란 및 가시광·근적외 합계, 시드 재현성 및 야간 영값을 확인한다. 부분 구름과 액체·빙정·눈이 포함된 장면을 중첩 옵션 0~3으로 검사한다. `test/rrtmgp/standalone_wrf_error.f90`는 독립 시험에만 쓰는 오류 처리 대체 함수다.

NOAA 사례 및 직접 코드 근거는 [NOAA 적용 사례](NOAA.md)에 정리했다. 실행 검증 결과는 [검증 기록](VALIDATION.md)에 기록한다.

## WRF 단일 컬럼 실행

기존 WRF configure 메뉴의 GNU serial 구성에서 `em_scm_xy`를 빌드한다. 본 환경은 `/bin/csh`가 없어 PATH의 csh로 compile을 호출했다.

```bash
export NETCDF="$PWD/build/deps/netcdf"
export NETCDF_classic=1
export LD_LIBRARY_PATH="$NETCDF/lib:${LD_LIBRARY_PATH:-}"
cd WRF
printf '32\n0\n' | ./configure
csh -f ./compile -j 12 em_scm_xy
cd ..
WRF/test/rrtmgp/run_scm.sh build/scm37 37
WRF/test/rrtmgp/run_scm.sh build/scm4 4
```

GNU serial 메뉴 번호는 이 플랫폼의 v4.8.0 configure 기준이다. 다른 플랫폼에서는 메뉴를 확인해 선택한다. 실행 스크립트는 새 작업 디렉터리에 WRF SCM 원본 초기 자료와 계수·테이블 링크를 배치하고 1999년 10월 22일 19 UTC부터 시간 간격 10초로 5분을 실행한다. 위의 의존성 경로는 이 작업 공간에서 준비한 로컬 경로이며 다른 환경에서는 설치한 NetCDF 경로를 지정한다.
