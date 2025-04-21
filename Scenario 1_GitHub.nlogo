extensions [profiler csv vid]
globals [number id id2 fbijxx fbijxx2 fbijyy fbijyy2 density_obs1 density_obs2 density_obs3 density_obs4 density_obs5 density_obs6 density_obs11 density_obs12 density_obs13 density_obs14 density_obs15
  exit_density ticks-recorder my-patches my-patches_beg cmax_exit cmax_beg u_avr outputfile vertices view-number]

turtles-own [ r
              goal
              rij
              vi
              vdes
              mass
              age
              Fdes
              Fdes_tsu
              Fbiw
              Fbij
              Fbi_obs
              Fsij
              Fsiw
              Fsi_obs
              sigmoid
              coord_FOV-agents
              rj
              FOV-walls
              FOV-obs
              dist_sz
              my-neighbors
              num-neighbors
              stress
              diwall
              high_stressed_k
              n_high_stressed_k
              neigh
              n
              contagion
              susceptibility
              crowd_pressure
              ]

patches-own [
  density
]

breed [adults adult]

to tsu
  if [pcolor] of patch-here = black [set pcolor blue]
end

to set-environment

  ;for walls in lime color
  ask patches with [(pxcor >= min-pxcor and pxcor <= max-pxcor and pycor = max-pycor)] [set pcolor lime + 1]
  ask patches with [(pxcor >= min-pxcor and pxcor <= max-pxcor and pycor = min-pycor)] [set pcolor lime + 1]

  ;to create-agents
  ask patches with [pycor >= min-pxcor + 1 and pycor <= max-pycor - 1 and pxcor =  min-pxcor] [set pcolor violet + 2]

  exit_type

  ;to calculate local density for each obstacle
  density_on_obstacle_9_6
  density_on_obstacle_13_9
  density_on_obstacle_13_3
  density_on_obstacle_17_6
  density_on_obstacle_21_9
  density_on_obstacle_21_3
  density_on_obstacle_37_3
  density_on_obstacle_37_9
  density_on_obstacle_41_6
  density_on_obstacle_45_3
  density_on_obstacle_45_9

  add_obstacles

end

to setup
   set-environment
  set ticks-recorder []
  if vid:recorder-status = "recording" [ vid:record-view ]
;;;;; to save csv file;;;; activate write-to-file in go procedure
;   let file user-new-file
;  ;; We check to make sure we actually got a string just in case
;  ;; the user hits the cancel button.
;  if is-string? file
;  [
;    ;; If the file already exists, we begin by deleting it, otherwise
;    ;; new data would be appended to the old contents.
;    if file-exists? file
;      [ file-delete file ]
;    file-open file
;    write-to-file
;  ]
   profiler:start
   reset-ticks
 end

;to ticks-for-threshold
;  if (count turtles = threshold) [error "stop creating agents"];ask turtle 0 [print age] stop] ; tocount how many ticks it takes to generate threshold agents
; end

to turtles-over-threshold
  carefully [if threshold = 50 and (count turtles > threshold) [ask turtle 50 [die]]] []
    carefully [if threshold = 50 and (count turtles > threshold) [ask turtle 51 [die]]] []
      carefully [if threshold = 50 and (count turtles > threshold) [ask turtle 52 [die]]] []
        carefully [if threshold = 50 and (count turtles > threshold) [ask turtle 53 [die]]] []

  carefully [if threshold = 100 and (count turtles > threshold) [ask turtle 100 [die]]][]
    carefully [if threshold = 100 and (count turtles > threshold) [ask turtle 101 [die]]][]
      carefully [if threshold = 100 and (count turtles > threshold) [ask turtle 102 [die]]][]
        carefully [if threshold = 100 and (count turtles > threshold) [ask turtle 103 [die]]][]

  carefully [if threshold = 200 and (count turtles > threshold) [ask turtle 200 [die]]][]
    carefully [if threshold = 200 and (count turtles > threshold) [ask turtle 201 [die]]][]
      carefully [if threshold = 200 and (count turtles > threshold) [ask turtle 202 [die]]][]
        carefully [if threshold = 200 and (count turtles > threshold) [ask turtle 203 [die]]][]

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to go
if ticks = 0 [reset-timer]
go-crono
  if (count turtles < threshold) AND (random 100 > 50) AND (ticks < tickss); put number here to avoid creating more turtles once start dying. Other option is to set a number just before turtle start dying, this is more flexibile when thinking about switching btw modes
    [repeat random 10 [create_adults] ;50 for 100>1 ;5;10/5030/0
  if (count turtles < threshold - 1) AND (ticks > tickss) [;1150 for 100 env1 1750 for 200 env1
    repeat random 0 [create_adults]
      ]
;    if (count turtles < threshold) AND (random 100 > 30) AND (ticks < tickss); put number here to avoid creating more turtles once start dying. Other option is to set a number just before turtle start dying, this is more flexibile when thinking about switching btw modes
;    [repeat random 10 [create_adults_females] ;50 for 100>1
;  if (count turtles < threshold - 1) AND (ticks > tickss) [;1150 for 100 env1 1750 for 200 env1
;    repeat random 0 [create_adults_females]
;      ]  ]

    ;  carefully [if ticks-for-threshold? [tick]] [print error-message stop];[print ticks]
      ;if (count turtles = threshold) [show ticks]; print count turtles]
       ;if (count turtles > threshold) [show ticks stop]
          ]
  turtles-over-threshold
  ;paint-in-radius
  ;if (count turtles = threshold) [stop]
  pen-down-report
  move
  ask turtles [
    time_stamp                                                                                                            ; assign age to turtles
    if pcolor = lime + 1 [print who info_before_dying die]
    ;if pcolor = lime + 1 [print who die]
    if pcolor = brown + 2 [set ticks-recorder lput ticks ticks-recorder die];]                                            ;print ticks
    recolor
    ;tsu
    if (env-type = 1) [avoid_trapped_agents]
    ;avoid_trapped_agentsII
    if (env-type = 2) [avoid_trapped_agents avoid_trapped_agentsII];avoid_trapped_agents_wall]
     ;if (env-type = 4) [avoid_trapped_agentsIII]
    distance-to-walls
     ]
  ;ask turtles [recolorII]
  ;ask turtles [avoid_trapped_agentsII]
  ;ask turtles [turtles_trapped_be]
  tick
  ;export-view (word ticks ".png") ; this is for exporting many png images. Useful for creating GIF in case video does not work
  ;;;;;;;;;;to stop simulation;;;;;;;;;;;;;;
  if not any? turtles and ticks > 10 [stop]
 ; if count turtles < 10 and ticks > 4000 [stop] ; in the meantime this is a way to stop the simulation when agents get stuck close to the exit
  if ticks > 20000 [stop]; use this when not kill agents stucked in walls
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  exit-density
  density-obs1
  density-obs2
  density-obs3
  density-obs4
  density-obs5
  density-obs6
  ;density-obs11
  density-obs12
  density-obs13
  density-obs14
  density-obs15
  ;output_exit
  ;output_beg
  if vid:recorder-status = "recording" [ vid:record-view ]          ; to record video
  ;write-to-file
  ;write-to-file_II
  ;print profiler:report ;; see results
;  if ticks >= 800 [
;     write-to-file_II
;    stop]
  ;export-output "output_test.csv" ; writes the contents of the model's output area. (If the model does not have a separate output area, the output portion of the Command Center is used.)
  ;export-world "test.csv" ; export the current state of the model. All turtles-own variables
  ;export-all-plots "plots.csv" ; export all the data contained in the plot (time series)
  end

to info_before_dying
  set number count turtles with [pcolor = lime  + 1]
set id [who] of turtles with [pcolor = lime  + 1]
  set id2 [who] of turtles with [item 0 Fbij > 0 and item 1 Fbij > 0]
set fbijxx [item 0 Fbij] of turtles with [pcolor = lime  + 1]
  set fbijxx2 [item 0 Fbij] of turtles with [item 0 Fbij > 0]
set fbijyy [item 1 Fbij] of turtles with [pcolor = lime  + 1]
  set fbijyy2 [item 1 Fbij] of turtles with [item 1 Fbij > 0]
end

to go-crono                                                                                ; running the counter

ask patch (min-pxcor + 9) (max-pycor) [set plabel ( precision (ticks / fps) 0 ) set plabel-color black]
ask patch (min-pxcor + 7) (max-pycor) [set plabel "Time in model (s)" set plabel-color black]
end

to create_adults
  let p one-of patches with [(pycor >= min-pxcor + 1 and pycor <= max-pycor - 1 and pxcor =  min-pxcor) and pcolor = violet + 2]                              ; let defines variables; != inequality
  ask p [ if not any? turtles in-radius 1
        [sprout-adults 1
          [ set shape "agent"
            set color cyan;one-of base-colors; - 3;set color magenta
            set heading 90
        if (exit-type = 0)[set goal one-of patches with [(pycor > min-pycor + 4 and pycor <= max-pycor - 6 and pxcor = max-pxcor) and pcolor = brown + 2]];3,5
        if (exit-type = 1)[set goal one-of patches with [pycor > min-pycor + 3 and pycor <= max-pycor - 5 and pxcor = max-pxcor and pcolor = brown + 2]] ; tsu
        if (exit-type = 2)[set goal one-of patches with [(pycor = min-pycor + 5 and pxcor = max-pxcor) or (pycor = max-pycor - 5 and pxcor = max-pxcor) and pcolor = brown + 2]]; tsu
        if (exit-type = 3)[set goal one-of patches with [(pycor > min-pycor and pycor < max-pycor and pxcor = max-pxcor) and pcolor = brown + 2]]
        if (exit-type = 4)[set goal one-of patches with [(pycor > min-pycor + 3 and pycor <= max-pycor - 5 and pxcor = max-pxcor) and pcolor = brown + 2]];3,5
            ;set vi (list (random-float 1.5) 0)
            ;set vi (list (0.66) 0)
           ;set vdes (list (vdes_tsu + random-float 0.34) 0)
            set vi (list (0.60 + (random-float 0.1)) 0)   ;from secondary data youtube video --> vo= 0.66
            set r ((random-normal 0.466 0.031) / 2)        ; Average biacromial breadth for both female and male from Viviani (2020)
            set size (2 * r) ; 1 patch 1 m ; 0.7
            set mass 78.45                                 ;mean of both male and female acc to Viviani (2020); 74.15 simple mean
         ]
       ]
      ]
end

to move
  ask turtles
 [
  if Fdes_K? [calculate-desired-force_karamouzas]
  if Fdes_SFM? [calculate-desired-force_SFM]
  calculate-social-force_ij
  calculate-social-force_iw
  calculate-social-force_i_obs
  calculate-body-force_ij
  calculate-body-force_iw
  calculate-body-force_i_obs
  calculate-desired-force_tsu
  calculate-new-u
  fd module vi / fps
]
end
;
;;***********************************************************************************************
;
to calculate-desired-force_karamouzas                                                                             ;from Mas et al. (2012)
 let ngi (list ([pxcor] of goal - xcor) ([pycor] of goal - ycor) )
 ;print ngi
 set ngi (list ((item 0 ngi) / module ngi) ((item 1 ngi) / module ngi))                                ;ngi = (gi - xi)/|gi-xi|
 set Fdes (list (1.28 * (vdes_notsu * (item 0 ngi) - (item 0 vi))) (0.25 * (vdes_notsu * (item 1 ngi) - (item 1 vi))))  ;Fg = 1/T(u-pref*ngi-v) (m/s2) Tacc=Vmax/a=1.8/1.4 (s)
 ;print vi
 ;print Fdes
;print (list "#: " who "Fdes:" Fdes "tick:" ticks)
 end
;;;
;;;;;***********************************************************************************************
;;;;;

to calculate-desired-force_SFM
 let ngi (list ([pxcor] of goal - xcor) ([pycor] of goal - ycor) )
 ;print ngi
 set ngi (list ((item 0 ngi) / module ngi) ((item 1 ngi) / module ngi))                                ;ngi = (gi - xi)/|gi-xi|
 ;print ngi
 ;if mass < 115 [
  ;set Fdes (list ((mass / Tr) * 0.00853 * ( 1 * vdes_notsu * (item 0 ngi) - (item 0 vi))) ((mass / Tr) * 0.0017 * ( 1 * vdes_notsu * (item 1 ngi) - (item 1 vi))))
;]    ; considering average mass for adults = 74.15 and Tr=0.5/0.0086,0.0017

   set Fdes (list ((1 * vdes_notsu * (item 0 ngi) - (item 0 vi)) / Tr) (( 1 * vdes_notsu * (item 1 ngi) - (item 1 vi))/ Tr))
   ;set Fdes (list ((mass / Tr) * 0.0055 * ( 1 * vdes_notsu * (item 0 ngi) - (item 0 vi))) ((mass / Tr) * 0.0010 * ( 1 * vdes_notsu * (item 1 ngi) - (item 1 vi)))) ; so far this is working okay for outliers
 ;set Fdes (list ((mass / Tr) * 1.28 /(mass / Tr) * ( 1 * vdes_notsu * (item 0 ngi) - (item 0 vi))) ((mass / Tr) * 0.25 /(mass / Tr) * ( 1 * vdes_notsu * (item 1 ngi) - (item 1 vi))))    ; considering average mass for adults = 74.15 and Tr=0.5/0.0086,0.0017
  ;print Fdes
  ;if mass >= 115 [
  ;set Fdes (list ((mass / Tr) * 0.0055 * ( 1 * vdes_notsu * (item 0 ngi) - (item 0 vi))) ((mass / Tr) * 0.0010 * ( 1 * vdes_notsu * (item 1 ngi) - (item 1 vi))))
;]    ; for outliers mass >115
;print (list "#: " who "Fdes:" Fdes "tick:" ticks)
 ; show mean item 0 [Fdes] of turtles
 end
;;;;
;;;;;***********************************************************************************************
;;;;;

to FOV [d a]                                                                                            ;Field of View : updates agents and obstacles in sight
if show-FOV? [ ask patches with [pcolor = sky] [ set pcolor black ]
               ask patches in-cone d a [ if pcolor = black [ set pcolor sky ] ]                         ;in-cone example https://stackoverflow.com/questions/37797177/how-to-ask-turtles-to-avoid-a-patch-with-specific-color-at-patch-ahead-1-but-tur?rq=1
             ]
  ;set FOV-agents [who] of other turtles in-radius d                                                    ;search for other agents. [who] prints a list
;set FOV-walls [list pxcor pycor] of other patches in-radius d with [pcolor = lime + 1]
  ;set FOV-obs [list pxcor pycor] of other patches in-radius d with [pcolor = lime + 2]
end
;;;;
;;;;;************************************************************************************************
;;;;;
to calculate-social-force_iw    ;for walls
  ;FOV 1 * FOV_walls angle_walls; ;1 * FOV_obj angle
  ;FOV 1 360
  let Fsiw_% [ ]
  let niw [ ]
  set FOV-walls [list pxcor pycor] of other patches in-radius (1 * FOV_walls) with [pcolor = lime + 1]
  ;show FOV-walls
  let diw map [ ?1 -> distance-nowrap patch item 0 ?1 item 1 ?1 ] FOV-walls
  ;print diw
  ;let niw_numm map [ ?1 -> distance-nowrap patch item 0 ?1 item 1 ?1 ] FOV-walls; is the same than xcor-item0?1 (below)
  ;print niw_numm
  let niw_num map [ ?1 -> (list (xcor - (item 0 ?1)) (ycor - (item 1 ?1))) ] FOV-walls
  ;print niw_num
  set niw map [ ?1 -> (list ((item 0 ?1) / module ?1) ((item 1 ?1) / module ?1)) ] niw_num
  ;print niw_num
  foreach diw
[ ?1 ->
   set Fsiw_% lput (Aiw * exp((r - ?1) / Biw)) Fsiw_%
  ]
 let Fsiw-list (map [ [?1 ?2] -> (list (?1 * item 0 ?2) ( ?1 * item 1 ?2)) ] Fsiw_% niw)
 let FsiwX 0
 let FsiwY 0
 foreach Fsiw-list [ ?1 -> set FsiwX FsiwX + item 0 ?1
                  set FsiwY FsiwY + item 1 ?1
                ]
 set Fsiw list FsiwX FsiwY
 ;output-show Fsiw
  ;if item 1 Fsiw > 0.4  [show Fsiw]
  ;if item 1 Fsiw < -0.4 [show Fsiw]
 ; output-print Fsiw
;show Fsiw
  ;show [fsiw] of turtle 0
end

;;;;;***********************************************************************************************
;;;;
to calculate-social-force_i_obs    ;this is working okay
  ;FOV 1 * FOV_obs angle_obs ;5 200;   FOV 5 200;
  let Fsi_obs% [ ]
  let niw_obs [ ]
  set FOV-obs [list pxcor pycor] of other patches in-radius (1 * FOV_obs) with [pcolor = lime + 2]
  let diw_obs map [ ?1 -> distance-nowrap patch item 0 ?1 item 1 ?1 ] FOV-obs
  ;print diw_obs
  ;let niw_numm map [ ?1 -> distance-nowrap patch item 0 ?1 item 1 ?1 ] FOV-walls; is the same than xcor-item0?1 (below)
  ;print niw_numm
  let niw_obs_num map [ ?1 -> (list (xcor - (item 0 ?1)) (ycor - (item 1 ?1))) ] FOV-obs
  ;print niw_num
  set niw_obs map [ ?1 -> (list ((item 0 ?1) / module ?1) ((item 1 ?1) / module ?1)) ] niw_obs_num
  ;print niw_num
  foreach diw_obs
[ ?1 ->
   set Fsi_obs% lput (Aiw * exp((r - ?1) / Biw)) Fsi_obs%
  ]
 let Fsi_obs-list (map [ [?1 ?2] -> (list (?1 * item 0 ?2) ( ?1 * item 1 ?2)) ] Fsi_obs% niw_obs)                 ;list of Fw*normal
 let FsiX_obs 0
 let FsiY_obs 0
 foreach Fsi_obs-list [ ?1 -> set FsiX_obs FsiX_obs + item 0 ?1                                                    ;sum of Fws ---> Fw = [FwX,FwY]
                  set FsiY_obs FsiY_obs + item 1 ?1
                ]
 set Fsi_obs list FsiX_obs FsiY_obs
  ;show Fsiw_obs
end

;print (list "turtle" who "tick:" ticks "dij
;;;;;***********************************************************************************************
;;;;
to calculate-social-force_ij
  let Fsij_% [ ]
  set coord_FOV-agents [list xcor ycor] of other turtles in-radius (1 * FOV_radius)
  let dij map [ ?1 ->  list (xcor - item 0 ?1) (ycor - item 1 ?1)] coord_FOV-agents  ;1 Ai=5
  ;carefully [if item 0 dij > 0.2 and item 0 dij < 0.6 [separate]] [] ; avoiding error bc of list dij [] length=0
  set dij map [ ?1 -> sqrt (((item 0 ?1)^ 2) + ((item 1 ?1)^ 2))] dij ; distance btw i and j's
  ;show dij
  let nij_num map [ ?1 -> list (xcor - item 0 ?1) (ycor - item 1 ?1)] coord_FOV-agents
  ;print nij_num
  carefully [set nij_num map [ ?1 -> list ((item 0 ?1) / module ?1) ((item 1 ?1) / module ?1)] nij_num] [] ; here I am suppresing the error when to agents are in the same location
  ;print nij
  set rj [r] of other turtles in-radius (1 * rj_radius)
  foreach rj
 [ ?1 ->
  set rij [r] of self + ?1
  ;print rij
  ]
  foreach dij
[ ?2 ->
    ;let neighborss other turtles in-radius 2
    ;set neighborss min-one-of neighborss [distance myself]
    ;if ?2 <= rij [set ?2 ?2 = rij]
    ;print [vi] of neighborss
    ;set Fsij_% lput (Ai * exp((rij - module ?2)/ Bi)) Fsij_%
    set Fsij_% lput (Ai * exp((rij - ?2)/ Bi)) Fsij_%
    ;let asd (rij - ?2)
    ;if asd > 0 [show asd inspect turtle who]
  ]
 let Fsij-list (map [ [?1 ?2] -> (list (?1 * item 0 ?2) ( ?1 * item 1 ?2)) ] Fsij_% nij_num)
 let FsijX 0
 let FsijY 0
 foreach Fsij-list [ ?1 -> set FsijX (FsijX + item 0 ?1)
                          set FsijY (FsijY + item 1 ?1)
                ]
 set Fsij list FsijX FsijY

end

;;;;;***********************************************************************************************
;;;;
to separate
   let neighborss other turtles in-radius 2
      set neighborss min-one-of neighborss [distance myself]; the closest neigh
      ;set neighborss neighborss [distance myself]; the closest neigh
  set heading [heading] of neighborss - 45
end
;;;;;***********************************************************************************************
;;;;

to calculate-body-force_ij     ;physical body force when peds are in contact with other peds
  FOV 1 * FOV_radius angle;FOV 5 100
  let Fbij_% [ ]
  let kn 120000
  set coord_FOV-agents [list xcor ycor] of other turtles in-radius (1 * FOV_radius)
  let dij map [ ?1 ->  list (xcor - item 0 ?1) (ycor - item 1 ?1)] coord_FOV-agents  ;1 Ai=5
  ;carefully [if item 0 dij > 0.2 and item 0 dij < 0.6 [separate]] [] ; avoiding error bc of list dij [] length=0
  set dij map [ ?1 -> sqrt (((item 0 ?1)^ 2) + ((item 1 ?1)^ 2))] dij ; distance btw i and j's
  ;show dij
  let nij_num map [ ?1 -> list (xcor - item 0 ?1) (ycor - item 1 ?1)] coord_FOV-agents
  ;print nij_num
  ;carefully [
  set nij_num map [ ?1 -> list ((item 0 ?1) / module ?1) ((item 1 ?1) / module ?1)] nij_num
  ;] [] ; here I am suppresing the error when to agents are in the same location
  ;print nij
  set rj [r] of other turtles in-radius (1 * rj_radius)
  foreach rj
 [ ?1 ->
  set rij [r] of self + ?1
  ;print rij
  ]
  foreach dij
  [ ?1 -> ifelse (rij > ?1)
  [ set Fbij_% lput (kn * (rij - ?1) / mass) Fbij_% ]
  [ set Fbij_% lput 0 Fbij_% ]
 ]

let Fbij-list (map [ [?1 ?2] -> (list (?1 * item 0 ?2) (?1 * item 1 ?2)) ] Fbij_% nij_num)
  let FbijX 0
  let FbijY 0
foreach Fbij-list [ ?1 -> set FbijX FbijX + item 0 ?1                                       ;sum of Fws ---> Fw = [FwX,FwY]
                          set FbijY FbijY + item 1 ?1
                ]
set Fbij list FbijX FbijY
  ;if item 0 Fbij > 0 or item 0 Fbij < 0 [print (word "Fbij check turtle " who)]
  ;show Fbij
end
;;;;;***********************************************************************************************
;;;;

to calculate-body-force_i_obs                                                                              ;physical body force when peds are in contact with fixed obstacles
FOV 1 * FOV_obs angle_obs;FOV 5 100
let Fbi_obs_% [ ]
let kn 120000                                                                                            ;[kg/sg2] don't forget about the paper from Sticco et al where they question this number
let ni_obs [ ]
set FOV-walls [list pxcor pycor] of other patches in-radius (1 * FOV_obs) with [pcolor = lime + 2] ; d
let di_obs map [ ?1 -> distance-nowrap patch item 0 ?1 item 1 ?1 ] FOV-obs
;print diw
let ni_obs_num map [ ?1 -> (list (xcor - (item 0 ?1)) (ycor - (item 1 ?1))) ] FOV-obs
set ni_obs map [ ?1 -> (list ((item 0 ?1) / module ?1) ((item 1 ?1) / module ?1)) ] ni_obs_num
;print niw_num

foreach di_obs
[ ?1 -> ifelse (r > ?1)
      ;[ set Fbiw_% lput (kn * (r - ?1)) Fbiw_% ]
  [ set Fbi_obs_% lput (kn * (r - ?1) / mass) Fbi_obs_% ]
  [ set Fbi_obs_% lput 0 Fbi_obs_%  ]
 ]

let Fbi_obs-list (map [ [?1 ?2] -> (list (?1 * item 0 ?2) (?1 * item 1 ?2)) ] Fbi_obs_% ni_obs)
  let Fbi_obsX 0
  let Fbi_obsY 0
foreach Fbi_obs-list [ ?1 -> set Fbi_obsX Fbi_obsX + item 0 ?1                                       ;sum of Fws ---> Fw = [FwX,FwY]
                          set Fbi_obsY Fbi_obsY + item 1 ?1
                ]
set Fbi_obs list Fbi_obsX Fbi_obsY
   ;if item 0 Fbiw > 0 or item 0 Fbiw < 0 [print (word "Fbiw check turtle " who)]
 ;show Fbiw
end

to calculate-body-force_iw                                                                              ;physical body force when peds are in contact with walls or fixed obstacles
;FOV 1 * FOV_walls angle_walls;FOV 5 100
let Fbiw_% [ ]
let kn 120000                                                                                            ;[kg/sg2] don't forget about the paper from Sticco et al where they question this number
let niw [ ]
set FOV-walls [list pxcor pycor] of other patches in-radius (1 * FOV_walls) with [pcolor = lime + 1] ; d
let diw map [ ?1 -> distance-nowrap patch item 0 ?1 item 1 ?1 ] FOV-walls
;print diw
let niw_num map [ ?1 -> (list (xcor - (item 0 ?1)) (ycor - (item 1 ?1))) ] FOV-walls
set niw map [ ?1 -> (list ((item 0 ?1) / module ?1) ((item 1 ?1) / module ?1)) ] niw_num
;print niw_num

foreach diw
[ ?1 -> ifelse (r > ?1)
      ;[ set Fbiw_% lput (kn * (r - ?1)) Fbiw_% ]
  [ set Fbiw_% lput (kn * (r - ?1) / mass) Fbiw_% ]
  [ set Fbiw_% lput 0 Fbiw_%  ]
 ]

let Fbiw-list (map [ [?1 ?2] -> (list (?1 * item 0 ?2) (?1 * item 1 ?2)) ] Fbiw_% niw)
  let FbiwX 0
  let FbiwY 0
foreach Fbiw-list [ ?1 -> set FbiwX FbiwX + item 0 ?1                                       ;sum of Fws ---> Fw = [FwX,FwY]
                          set FbiwY FbiwY + item 1 ?1
                ]
set Fbiw list FbiwX FbiwY
   ;if item 0 Fbiw > 0 or item 0 Fbiw < 0 [print (word "Fbiw check turtle " who)]
 ;show Fbiw
end

;;;;;***********************************************************************************************
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;TO AVOID TURTLES TRAPPED ON OBS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to avoid_trapped_agents
   if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and [ycor] of self = [pycor] of goal and (item 1 vi = 0) and count turtles in-radius 0.5 = 1)
  [set heading heading - 90] ;0
    fd 0.001 ;this makes th simulaiton go way to fast. its takes 1750 ticks to create 200 agents while when deactivated it takes

;  if (any? patches in-cone 0.5 180 with [pcolor = lime + 2] and (item 0 vi = 0))
;  [set heading heading - 90];180
    ;fd 0.015 ;for extreme cases
 end

to avoid_trapped_agentsIII
   if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and count turtles = 1 and age > 1500 ) ;and [ycor] of self > [pycor] of goal
  [set heading 180];heading - 90] ;0
    fd fdd

  ;if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and [ycor] of self < [pycor] of goal and count turtles in-radius 0.5 = 1)
  ;[set heading 0];heading - 90] ;0
  ;  fd fdd;0.001
 end

to turtles_trapped_be
    let turtles_trapped count turtles-on patches with [pcolor = cyan + 5]
  ;;print turtles_trapped
  ;;if (turtles_trapped = count turtles) [die]
;   if (ticks > 1400 and any? patches in-cone 1.5 180 with [pcolor = lime + 2] and ([ycor] of self >= 5.43 and [ycor] of self < 5.52))
;  [set heading 180]
;   if (ticks > 4000 and any? patches in-cone 1.5 180 with [pcolor = lime + 2] and ([ycor] of self >= 5.43 and [ycor] of self < 5.52))
;  [set heading 180 fd fdd]
;  if (ticks > 1400 and any? patches in-cone 1.5 180 with [pcolor = lime + 2] and [ycor] of self >= 6.45 and [ycor] of self < 6.56)
;  [set heading heading - 90]
   if count turtles <= threshold * 0.2 and turtles_trapped <= 6 [avoid_trapped_agentsII];avoid_trapped_agents_walls2]; and turtles_trapped > 0
end

;;;;;***********************************************************************************************
;;;;
to avoid_trapped_agentsII

   ;;if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and ([pycor] of self > [pycor] of goal)) [set heading 360 fd fdd];>
  ;;if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and ([pycor] of self = 5)) [set heading 180 fd fdd];0.08];
  if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and ([ycor] of self > 6.40 and [ycor] of self <= 7) and age > 450) [set heading 0 fd fdd]; 0.160 risking  having higher Fbij which I think is the case
  if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and ([ycor] of self > 5.0 and [ycor] of self <= 5.8) and age > 450) [set heading 180 fd fdd];];0.160 risking  having higher Fbij which I think is the case
   ;;if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and ([pycor] of self < [pycor] of goal)) [set heading 180 fd fdd];<
    ;if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and (xcor <= 3.5 and pycor <= 2 )) [set heading 0]; fd fdd]; for FOV=1
  ;if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and (pycor = 1 and pxcor = 2 )) [set heading 0]; fd fdd]; for FOV=1
;;if (any? patches in-cone 1.5 180 with [pcolor = lime + 2] and ([ycor] of self > 5.0 and [ycor] of self <= 5.8)) [set heading 180]; fd fdd]
end

to avoid_trapped_agents_walls2

  if [pcolor] of patch-here = cyan + 5 and ([pycor] of self > [pycor] of goal) [set heading 360 fd fdd];and (item 1 Fsiw_obs = 0) [fd 0.15]; [set heading 360]
  if [pcolor] of patch-here = cyan + 5 and ([pycor] of self < [pycor] of goal) [set heading 180 fd fdd]; [set heading 180]
;   ;and (ycor < 9.51 and ycor > 9.48) or (ycor < 8.51 and ycor > 8.48) or (ycor < 7.51 and ycor > 7.48) or (ycor < 6.51 and ycor > 6.48))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;to not use the slider;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  if [pcolor] of patch-here = cyan + 5 and ([pycor] of self > [pycor] of goal) and vdes_notsu = 0.66 [set heading 360 fd 0.11]; 0.10 > fd > 0.15
;  if [pcolor] of patch-here = cyan + 5 and ([pycor] of self < [pycor] of goal) and vdes_notsu = 0.66 [set heading 180 fd 0.11]
;
;   if [pcolor] of patch-here = cyan + 5 and ([pycor] of self > [pycor] of goal) and vdes_notsu = 1 [set heading 360 fd 0.15] ; 0.10 > fd > 0.15
;  if [pcolor] of patch-here = cyan + 5 and ([pycor] of self < [pycor] of goal) and vdes_notsu = 1 [set heading 180 fd 0.15]
;
;   if [pcolor] of patch-here = cyan + 5 and ([pycor] of self > [pycor] of goal) and vdes_notsu = 1.34 [set heading 360 fd 0.20] ; fd > 0.15
;  if [pcolor] of patch-here = cyan + 5 and ([pycor] of self < [pycor] of goal) and vdes_notsu = 1.34 [set heading 180 fd 0.20]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;if [pcolor] of patch-here = cyan + 5

  ;if (any? patches in-cone 1.5 60 with [pcolor = lime + 2]) ;and (item 0 vi > -1) and (item 0 vi < 1)
   ;and (ycor < 9.51 and ycor > 9.48) or (ycor < 8.51 and ycor > 8.48) or (ycor < 7.51 and ycor > 7.48) or (ycor < 6.51 and ycor > 6.48))
    ;and (ycor < 10 and ycor > 5))
  ;[fd 0.080]
   ;[set heading heading ;0.05 for des=0.66 // for vdes =2 and Biw=0.010 fd=0.068
  ;]
    ;if (any? patches in-cone 1.5 60 with [pcolor = lime + 2]) ;and (item 0 vi > -1) and (item 0 vi < 1)
    ;and (ycor < 5.515 and ycor > 5.48) or (ycor < 4.51 and ycor > 4.48) or (ycor < 3.51 and ycor > 3.48) or (ycor < 2.51 and ycor > 2.46))
    ;and (ycor < 4.99 and ycor > 2))
  ;[fd 0.080]
    ;[set heading heading
  ;]
end

;;;;;***********************************************************************************************
;;;;
to calculate-evac_stress
  let sz []
  if (exit-type = 0)[set sz (list ([list pxcor pycor] of goal))];one-of patches with [(pycor > min-pycor + 4 and pycor <= max-pycor - 6 and pxcor = 50) and pcolor = brown + 2]))];3,5
  if (exit-type = 1)[set sz (list ([list pxcor pycor] of goal))];one-of patches with [pycor > min-pycor + 3 and pycor <= max-pycor - 5 and pxcor = max-pxcor and pcolor = brown + 2]))] ; tsu
  if (exit-type = 2)[set sz (list ([list pxcor pycor] of one-of patches with [(pycor = min-pycor + 5 and pxcor = max-pxcor) or (pycor = max-pycor - 5 and pxcor = max-pxcor) and pcolor = brown + 2]))]
  if (exit-type = 4)[set sz (list ([list pxcor pycor] of one-of patches with [(pycor > min-pycor + 3 and pycor <= max-pycor - 5 and pxcor = max-pxcor) and pcolor = brown + 2]))];3,5
  ;print sz
  set dist_sz map [ ?1 -> distance-nowrap patch item 0 ?1 item 1 ?1 ] sz
  set sigmoid 1 / (1 + e ^ (beta * -1 * item 0 dist_sz))
  ;show [sigmoid] of turtles
  ;print [sigmoid] of turtles with [stress = "intermediate"]
  ;print min [sigmoid] of turtles with [stress = "intermediate"]
end

;;;;;***********************************************************************************************
to calculate-desired-force_tsu
  ;let Fdes_tsu [ ]
  let ngi (list ([pxcor] of goal - xcor) ([pycor] of goal - ycor) )
  set ngi (list ((item 0 ngi) / module ngi) ((item 1 ngi) / module ngi))
  ;if item 0 ngi < 0.9 [print (word "check ngi turtle " who)]
  ;print (list who ngi)
 ; set vdes (list (0.66) 0)
  set vdes (list (vdes_tsu + random-float 0.34) 0)
 ;show [vdes] of turtles`
  foreach vdes
  [ ?1 -> ;(ifelse sigmoid >= 0.9 [set vdes 2.77];
    ;(ifelse sigmoid >= 0.9 and count turtles > 0 [ask n-of (count turtles * percentage) turtles [set vdes 2.77]]
      ;[set vdes vdes_tsu + random-float 0.34];]

;      (ifelse sigmoid >= 0.9 and count turtles > 0 [ask n-of (count turtles * percentage) turtles [set vdes 2.77]]
    ;ifelse count turtles > threshold and sigmoid >= 0.9 [ask n-of 20 turtles [set vdes 2.77]][set vdes 2.77]
;    sigmoid > 0.1 and sigmoid < 0.9 [set vdes sigmoid * 2.2125 + 0.8]
;    [set vdes vdes_tsu + random-float 0.34]
;    )
;     print [vdes] of turtles
;]

    (ifelse stress = "high" [set vdes 2.77]
      stress = "intermediate" [set vdes sigmoid * 2.2125 + 0.8]
      [set vdes vdes_tsu + random-float 0.34])
    ; print [vdes] of turtles
]

;  (ifelse sigmoid >= 0.9
;[set vdes 2.77] ; average for running peds in Fraset et al 2014
;  sigmoid > 0.1 and sigmoid < 0.9
;[set vdes sigmoid * 2.2125 + 0.8]
;[set vdes vdes_tsu + random-float 0.34]
;)
  ;print vdes
  ;set color scale-color magenta vdes (max [vdes] of turtles + 3) 0.66
  ;set color scale-color magenta sigmoid (max [sigmoid] of turtles + 1) 0
  ;set Fdes_tsu list ((mass / Tr) * 1 * lambda0 * (vdes * (item 0 ngi) - (item 0 vi))/ (mass *  lambda0 )) ((mass / Tr) * lambda1 * (vdes * (item 1 ngi) - (item 1 vi)) / (mass *  lambda1))    ; considering average mass for adults = 74.15 and Tr=0.5/0.0086,0.0017.....0.0054/0.0086
  ;set Fdes_tsu list ((mass / Tr) * 1 * lambda0 * (vdes * (item 0 ngi) - (item 0 vi))) ((mass / Tr) * lambda1 * (vdes * (item 1 ngi) - (item 1 vi)))    ; considering average mass for adults = 74.15 and Tr=0.5/0.0086,0.0017.....0.0054/0.0086
  carefully [set Fdes_tsu list (1.28 * 0.6 * (vdes * (item 0 ngi) - (item 0 vi)) / Tr) (0.25 * 0.6 * (vdes * (item 1 ngi) - (item 1 vi))/ Tr)][]
 ; set Fdes_tsu (list ((1 * vdes * (item 0 ngi) - (item 0 vi)) / Tr) (( 1 * vdes * (item 1 ngi) - (item 1 vi))/ Tr)); Fdes
  ;print item 0 Fdes_tsu
  ;print item 1 Fdes_tsu
; print Fdes_tsu
;   let Fdes_tsuX 0
;  set Fdes_tsuX item 0 Fdes_tsu
;   let Fdes_tsuY 0
;   set Fdes_tsuY item 1 Fdes_tsu
;  set Fdes_tsu list Fdes_tsuX Fdes_tsuY
;  print Fdes_tsu
 ; show (list "Fdes_tsu->" Fdes_tsu)
end

to stress-level
  ifelse sigmoid >= 0.9 [set stress "high"]; 2] ;(word "high")]
        [ifelse sigmoid > 0.1 and sigmoid < 0.9  [set stress "intermediate"];1]; (word "intermediate")]
          [set stress "low"];0];(word "low")]
          ;if stress = "intermediate" and num-neighbors >= 4 and diwall = "close" [set stress "high"];for "intermediate case" scenario
  ]
     ;show [stress] of turtles
end

to distance-to-walls
  set FOV-walls [list pxcor pycor] of other patches in-radius (1 * FOV_walls) with [pcolor = lime + 1]
  let diw map [ ?1 -> distance-nowrap patch item 0 ?1 item 1 ?1 ] FOV-walls
  ;print diw
  carefully [ifelse item 0 diw <= 1 [set diwall "close"]
    [set diwall "far"]
  ][]
  ;if diwall = "close" [print diwall]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;TO GAVE DIFFERENT COLORS TO TURTLES BASED ON VDES ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor
;  ifelse sigmoid >= 0.9
;  [set color magenta]
;  [ifelse sigmoid < 0.9 and sigmoid > 0.1
;    [set color cyan]
;    [set color white]
;  ]
;ifelse vdes >= 2.77
;  [set color magenta]
;  [ifelse vdes < 2.77 and vdes > 2
;    [set color cyan]
;    [set color white]
;  ]

  ifelse stress = "high"
  [set color magenta]
  [ifelse stress = "intermediate"
    [set color cyan]
    [set color white]
  ]

;  ifelse num-neighbors >= 4
;  [set color magenta]
;  [ifelse num-neighbors < 4 and num-neighbors > 2
;    [set color cyan]
;    [set color white]
;  ]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;TO GAVE DIFFERENT COLORS TO TURTLES BASED ON VDES ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolorII
;  ifelse item 0 Fsiw = 0
;  [set color yellow + 5]
;  [ifelse item 0 Fsiw > 0.1
;    [set color magenta]
;        [set color cyan]
;        ]
   ifelse item 0 Fsi_obs = 0
  [set color white]
  [ifelse item 0 Fsi_obs > 0
    [set color magenta + 3]
        [set color cyan + 3]
        ]
end
;;;;;***********************************************************************************************
;;;;
to-report u-des
;print (list "tick:" ticks " Fdes: " Fdes "#: " who " vi: " vi "tick:" ticks)
;print (list "#: " who " vi: " vi "tick:" ticks " Fdes: " Fdes " Fsiw_obs: " Fsiw_obs " Fsij: " Fsij "Fbiw: " Fbiw  "Fdes_tsu: " Fdes_tsu)
;show (list "#: " who " vi: " vi " Fsiw: " Fsiw " Fsiw_obs: " Fsiw_obs " Fsij: " Fsij "Fdes_tsu: " Fdes_tsu);  "Fdes_tsu: " Fdes_tsu)
  ;let vix item 0 vi + item 0 Fbij + item 0 Fdes_tsu + item 0 Fsij + item 0 Fsiw + item 0 Fbiw; + item 0 Fbij ; + item 0 Fsi_obs ;+ item 0 Fbi_obs  + item 0 Fbiw; + item 0 Fdes_tsu; + item 0 Fdes; + item 0 Fdes_SFM ;+ item 0 Fsij + item 0 Fdes ;+ item 0 Fsiw ;+ item 0 Fdes_tsu ;+ item 0 Fsij ;; + item 0 Fdes
  ;let viy item 1 vi + item 1 Fbij + item 1 Fdes_tsu + item 1 Fsij + item 1 Fsiw + item 1 Fbiw; + item 1 Fbij ; + item 1 Fsi_obs ;+ item 1 Fbij + item 1 Fbi_obs + item 1 Fsiw + item 1 Fbiw; + item 1 Fdes_tsu; + item 1 Fdes; + item 1 Fdes_SFM ;+ item 1 Fsij + item 1 Fdes ;+ item 1 Fsiw ;+ item 1 Fdes_tsu ;+ item 1 Fsij ;+ item 1 Fbiw  ;+ item 1 Fdes
  let vix item 0 vi + item 0 Fdes + item 0 Fsij + item 0 Fsiw + item 0 Fsi_obs + item 0 Fbij + item 0 Fbi_obs + item 0 Fbiw ;+ item 0 Fdes;];[]
  let viy item 1 vi + item 1 Fdes + item 1 Fsij + item 1 Fsiw + item 1 Fsi_obs + item 1 Fbij + item 1 Fbi_obs + item 1 Fbiw ;+ item 1 Fdes
  ;show list vix viy
 ; print vix
 ; print viy
report list vix viy
 ; show u-des
end
;;;
to-report module [a] ;a [ x y]
  report sqrt ( (item 0 a) ^ 2 + (item 1 a) ^ 2 )
end
;;;
;;;;***********************************************************************************************
;;;
to calculate-new-u
set vi u-des
  ;print vi
;print (list "tick:" ticks "vi:" vi "#: " who  "Fsiw_obs:" Fsiw_obs  "Fsij:" Fsij  "Fsiw:" Fsiw  "Fdes_tsu:" Fdes_tsu )
set heading atan item 0 vi item 1 vi
 ;show (list "heading->"heading)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to exit_type

 if (exit-type = 0)[
    ask patches with [pycor > min-pycor + 4 and pycor <= max-pycor - 6 and pxcor = 50] [set pcolor brown + 2];3,5
  ;;;to calculate density near the exit_
   ask patches with [pycor = min-pycor + 4 and pxcor = 50] [set pcolor red + 5];3,5
   ask patches with [pycor = min-pycor + 7 and pxcor = 50] [set pcolor red + 5];3,5
   ask patches with [pycor > min-pycor + 3 and pycor <= max-pycor - 5 and pxcor = 49] [set pcolor red + 5]
  ]

   if (exit-type = 1)[
  ;ask patches with [pycor > min-pycor + 2 and pycor <= max-pycor - 3 and pxcor = max-pxcor] [set pcolor brown + 3]
  ask patches with [pycor > min-pycor + 3 and pycor <= max-pycor - 5 and pxcor = max-pxcor] [set pcolor brown + 2]; tsu
      ;;;to calculate density near the exit_tsu
   ask patches with [pycor = min-pycor + 3 and pxcor = max-pxcor] [set pcolor red + 5];3,5
   ask patches with [pycor = max-pycor - 4 and pxcor = max-pxcor] [set pcolor red + 5];3,5
   ask patches with [pycor > min-pycor + 2 and pycor <= max-pycor - 4 and pxcor = 49] [set pcolor red + 5]
  ]

   if (exit-type = 2)[
  ;ask patches with [pycor > min-pycor + 2 and pycor <= max-pycor - 3 and pxcor = max-pxcor] [set pcolor brown + 3]
   ask patches with [pycor = min-pycor + 5 and pxcor = max-pxcor] [set pcolor brown + 2]; tsu
   ask patches with [pycor = max-pycor - 5 and pxcor = max-pxcor] [set pcolor brown + 2]

      ;;;to calculate density near the exit_tsu
   ask patches with [pycor = min-pycor + 6 and pxcor = max-pxcor] [set pcolor red + 5];3,5
   ask patches with [pycor = min-pycor + 4 and pxcor = max-pxcor] [set pcolor red + 5];3,5
   ask patches with [pycor = max-pycor - 4 and pxcor = max-pxcor] [set pcolor red + 5];3,5
   ask patches with [pycor > min-pycor + 3 and pycor <= max-pycor - 4 and pxcor = 49] [set pcolor red + 5]
  ]

  if (exit-type = 3)[
    ask patches with [pycor > min-pycor and pycor < max-pycor and pxcor = max-pxcor] [set pcolor brown + 2];3,5
  ;;;to calculate density near the exit_
   ask patches with [pycor > min-pycor and pycor < max-pycor and pxcor = max-pxcor - 1] [set pcolor red + 5];3,5
  ]

   if (exit-type = 4)[
    ask patches with [pycor > min-pycor + 3 and pycor <= max-pycor - 5 and pxcor = max-pxcor] [set pcolor brown + 2];3,5

    ;;;to calculate density near the exit_
   ;ask patches with [pycor = min-pycor + 4 and pxcor = max-pxcor - 1] [set pcolor red + 3];3,5
   ;ask patches with [pycor = min-pycor + 7 and pxcor = max-pxcor - 1] [set pcolor red + 3];3,5
   ask patches with [pycor > min-pycor + 3 and pycor <= max-pycor - 5 and pxcor = max-pxcor - 1] [set pcolor red + 5]
  ]

end

to add_obstacles
    ;activate [avoid_trapped_agents]
  if (env-type = 0) []
  if (env-type = 1) [
  ;ask patches with [(pxcor = 14 and pycor = 2)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 14 and pycor = 10)] [set pcolor lime + 2];
  ask patches with [(pxcor = 9 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 13 and pycor = 9)] [set pcolor lime + 2];
  ask patches with [(pxcor = 13 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 17 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 21 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 25 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 21 and pycor = 9)] [set pcolor lime + 2];
  ask patches with [(pxcor = 29 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 29 and pycor = 9)] [set pcolor lime + 2];
  ask patches with [(pxcor = 33 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 37 and pycor = 9)] [set pcolor lime + 2];
  ask patches with [(pxcor = 37 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 41 and pycor = 6)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 42 and pycor = 5)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 42 and pycor = 7)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 43 and pycor = 4)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 43 and pycor = 8)] [set pcolor lime + 2];
  ask patches with [(pxcor = 45 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 45 and pycor = 9)] [set pcolor lime + 2];
  ]
   if (env-type = 2) [
  ask patches with [(pxcor = 9 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 13 and pycor = 9)] [set pcolor lime + 2];
  ask patches with [(pxcor = 13 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 17 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 21 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 25 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 21 and pycor = 9)] [set pcolor lime + 2];
  ask patches with [(pxcor = 29 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 29 and pycor = 9)] [set pcolor lime + 2];
  ask patches with [(pxcor = 33 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 37 and pycor = 9)] [set pcolor lime + 2];
  ask patches with [(pxcor = 37 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 41 and pycor = 6)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 42 and pycor = 5)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 42 and pycor = 7)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 43 and pycor = 4)] [set pcolor lime + 2];
  ;ask patches with [(pxcor = 43 and pycor = 8)] [set pcolor lime + 2];
  ask patches with [(pxcor = 45 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 45 and pycor = 9)] [set pcolor lime + 2];
    ;;;;;;;;;;;;;;;;;;;;;;;;;;To create a wall;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;before the exit --> Env 2;;;;;;;;;;;;;;;;;;;;;;
     ask patches with [(pxcor = 41 and pycor = 5)] [set pcolor lime + 2];
     ask patches with [(pxcor = 41 and pycor = 7)] [set pcolor lime + 2];
    ;;;;;;;;;;;;;;;;;;;;;;;Cyan patches before the wall;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;to make trapped_agents_be work;;;;;;;;;;;;;;;;
    ; ask patches with [(pxcor = 40 and pycor = 5)] [set pcolor cyan + 5];
    ;ask patches with [(pxcor = 40 and pycor = 6)] [set pcolor cyan + 5];
    ; ask patches with [(pxcor = 40 and pycor = 7)] [set pcolor cyan + 5];
    ;ask patches with [(pxcor = 39 and pycor = 5)] [set pcolor cyan + 5];
    ;ask patches with [(pxcor = 39 and pycor = 6)] [set pcolor cyan + 5];
    ; ask patches with [(pxcor = 39 and pycor = 7)] [set pcolor cyan + 5];
  ]
    if (env-type = 3) [
       ;for exit room env
  ask patches with [(pxcor >= min-pxcor and pxcor <= max-pxcor and pycor = max-pycor)] [set pcolor lime + 1]
  ask patches with [(pxcor >= min-pxcor and pxcor <= max-pxcor and pycor = min-pycor)] [set pcolor lime + 1]
  ask patches with [(pxcor = max-pxcor and pycor >= max-pycor - 5 and pycor < max-pycor)] [set pcolor lime + 1]
  ask patches with [(pxcor = max-pxcor and pycor <= min-pycor + 4 and pycor > min-pycor)] [set pcolor lime + 1]
  ]

  if (env-type = 4) [
      ;3 obstacles before the exit
  ask patches with [(pxcor = 40 and pycor = 6)] [set pcolor lime + 2];
  ask patches with [(pxcor = 44 and pycor = 3)] [set pcolor lime + 2];
  ask patches with [(pxcor = 44 and pycor = 9)] [set pcolor lime + 2];
    ;walls blocking "narrowing" the exit

   ask patches with [(pxcor = max-pxcor and pycor >= max-pycor - 4 and pycor < max-pycor)] [set pcolor lime + 1]
   ask patches with [(pxcor = max-pxcor and pycor <= min-pycor + 3 and pycor > min-pycor)] [set pcolor lime + 1]

  ;ask patches with [(pxcor >= min-pxcor and pxcor <= max-pxcor and pycor = max-pycor)] [set pcolor lime + 1]
  ;ask patches with [(pxcor >= min-pxcor and pxcor <= max-pxcor and pycor = min-pycor)] [set pcolor lime + 1]
  ;ask patches with [(pxcor = max-pxcor and pycor >= max-pycor - 5 and pycor < max-pycor)] [set pcolor lime + 1]
  ;ask patches with [(pxcor = max-pxcor and pycor <= min-pycor + 4 and pycor > min-pycor)] [set pcolor lime + 1]
  ]

end

to density_on_obstacle_9_6
  ask patches with [(pxcor = 9 and pycor >= 5 and pycor <= 7)] [set pcolor yellow + 5];
  ask patches with [(pxcor = 8 and pycor >= 5 and pycor <= 7 )] [set pcolor yellow + 5];
end

to density-obs1
  set density_obs1 count turtles-on patches with [pcolor = yellow + 5] / count patches with [pcolor = yellow + 5]
  ;print count turtles-on patches with [pcolor = yellow + 3]
 ;print count patches with [pcolor = yellow + 3]
  ;print density_obs1
  ;set density_obs1 count turtles-on patches with [(pxcor = 9 and pycor >= 5 and pycor <= 7) and (pxcor = 8 and pycor >= 5 and pycor <= 7 )] /
  ;count patches with [(pxcor = 9 and pycor >= 5 and pycor <= 7) and (pxcor = 8 and pycor >= 5 and pycor <= 7 )]
 end

to density_on_obstacle_13_9
ask patches with [(pxcor = 13 and pycor >= 8 and pycor <= 10)] [set pcolor green + 5];
  ask patches with [(pxcor = 12 and pycor >= 8 and pycor <= 10)] [set pcolor green + 5];
end

to density-obs2
  set density_obs2 count turtles-on patches with [pcolor = green + 5] / count patches with [pcolor = green + 5]
end

 to density_on_obstacle_13_3
  ask patches with [(pxcor = 13 and pycor >= 2 and pycor <= 4)] [set pcolor turquoise + 5];
  ask patches with [(pxcor = 12 and pycor >= 2 and pycor <= 4)] [set pcolor turquoise + 5];
end

to density-obs3
  set density_obs3 count turtles-on patches with [pcolor = turquoise + 5] / count patches with [pcolor = turquoise + 5]
end

to density_on_obstacle_17_6
  ask patches with [(pxcor = 17 and pycor >= 5 and pycor <= 7 )] [set pcolor orange + 5];
  ask patches with [(pxcor = 16 and pycor >= 5 and pycor <= 7 )] [set pcolor orange + 5];
end

to density-obs4
  set density_obs4 count turtles-on patches with [pcolor = orange + 5] / count patches with [pcolor = orange + 5]
end

to density_on_obstacle_21_9
  ask patches with [(pxcor = 21 and pycor >= 8 and pycor <= 10)] [set pcolor blue + 5];
  ask patches with [(pxcor = 20 and pycor >= 8 and pycor <= 10)] [set pcolor blue + 5];
end

to density-obs5
  set density_obs5 count turtles-on patches with [pcolor = blue + 5] / count patches with [pcolor = blue + 5]
end

to density_on_obstacle_21_3
  ask patches with [(pxcor = 21 and pycor >= 2 and pycor <= 4)] [set pcolor cyan + 5];
  ask patches with [(pxcor = 20 and pycor >= 2 and pycor <= 4)] [set pcolor cyan + 5];
end

to density-obs6
  set density_obs6 count turtles-on patches with [pcolor = cyan + 5] / count patches with [pcolor = cyan + 5]
end

to density_on_obstacle_37_9
  ask patches with [(pxcor = 37 and pycor >= 8 and pycor <= 10)] [set pcolor pink + 5];
  ask patches with [(pxcor = 36 and pycor >= 8 and pycor <= 10)] [set pcolor pink + 5];
end

to density-obs11
  set density_obs11 count turtles-on patches with [pcolor = pink + 5] / count patches with [pcolor = pink + 5]
end

to density_on_obstacle_37_3
  ask patches with [(pxcor = 37 and pycor >= 2 and pycor <= 4)] [set pcolor gray + 5];
  ask patches with [(pxcor = 36 and pycor >= 2 and pycor <= 4)] [set pcolor gray + 5];
end

to density-obs12
  set density_obs12 count turtles-on patches with [pcolor = gray + 5] / count patches with [pcolor = gray + 5]
end

to density_on_obstacle_41_6
  ask patches with [(pxcor = 41 and pycor >= 5 and pycor <= 7 )] [set pcolor sky + 5];
  ask patches with [(pxcor = 40 and pycor >= 5 and pycor <= 7 )] [set pcolor sky + 5];
end

to density-obs13
  set density_obs13 count turtles-on patches with [pcolor = sky + 5] / count patches with [pcolor = sky + 5]
end

to density_on_obstacle_45_9
  ask patches with [(pxcor = 45 and pycor >= 2 and pycor <= 4)] [set pcolor magenta + 5];
  ask patches with [(pxcor = 44 and pycor >= 2 and pycor <= 4)] [set pcolor magenta + 5];
end

to density-obs14
  set density_obs14 count turtles-on patches with [pcolor =  magenta + 5] / count patches with [pcolor =  magenta + 5]
end

to density_on_obstacle_45_3
 ask patches with [(pxcor = 45 and pycor >= 8 and pycor <= 10)] [set pcolor brown + 5];
  ask patches with [(pxcor = 44 and pycor >= 8 and pycor <= 10)] [set pcolor brown + 5];
end

 to density-obs15
  set density_obs15 count turtles-on patches with [pcolor = brown + 5] / count patches with [pcolor = brown + 5]
end

to density_on_obstacles

  ask patches with [(pxcor = 25 and pycor >= 5 and pycor <= 7 )] [set pcolor violet + 2];
  ask patches with [(pxcor = 24 and pycor >= 5 and pycor <= 7 )] [set pcolor violet + 2];
  ask patches with [(pxcor = 33 and pycor >= 5 and pycor <= 7 )] [set pcolor violet + 2];
  ask patches with [(pxcor = 32 and pycor >= 5 and pycor <= 7 )] [set pcolor violet + 2];

  ask patches with [(pxcor = 29 and pycor >= 8 and pycor <= 10)] [set pcolor violet + 2];
  ask patches with [(pxcor = 28 and pycor >= 8 and pycor <= 10)] [set pcolor violet + 2];

  ask patches with [(pxcor = 29 and pycor >= 2 and pycor <= 4)] [set pcolor violet + 2];
  ask patches with [(pxcor = 28 and pycor >= 2 and pycor <= 4)] [set pcolor violet + 2];

end

to time_stamp
  set age (age + 1)
end

to exit-density
  set exit_density count turtles-on patches with [pcolor = red + 5] / count patches with [pcolor = red + 5]
   ; let turtles_trapped count turtles-on patches with [pcolor = cyan + 5]
  ;print count turtles-on patches with [pcolor = red + 3]
  ;print count patches with [pcolor = red + 3]
  ;show exit_density
end

to do-plots_exit2
  set-current-plot "local density at exit"
plot exit_density
end

to do-plots_obs
  set-current-plot "local density at exit"
  plot density_obs1
  plot density_obs2
  plot density_obs3
  plot density_obs4
  plot density_obs5
  plot density_obs6
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EXPORT FILE IN .CSV ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;GO TO SETUP TO UNCOMMENT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;UNCOMMENT ALSO IN GO PROCEDURE;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to write-to-file
 ;      file-print (word "who; ticks; heading; xcor; ycor; Fsiw; vi; Fdes_tsu")
  ;; use SORT so the turtles print their data in order by who number,
  ;; rather than in random order
  foreach sort turtles [ t ->
    ask t [
     ; file-print (word self " ticks: " ticks " pxcor: " xcor " pycor: " ycor " Fsiw: " Fsiw); " Fsij: " Fsij " Fbiw: " Fbiw  " Fdes_tsu: " Fdes_tsu)
      ;file-print (word self " ; " ticks " ; "heading" ; " xcor " ; " ycor " ; " Fsiw " ; " vi " ; " Fdes_tsu )
      file-print (word self " ; " ticks " ; " cmax_exit" ; " Fsij " ; " vi " ; " Fdes_tsu); "; ; " vdes ) ; " xcor " ; " ycor " ; " Fsiw " ; " vi " ; " Fdes_tsu )
    ]
       ]
  ;file-print (word "self; ticks; pxcor; pycor; Fsiw; vi; Fdes_tsu")
  ;file-print ""  ;; blank line
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;TO RECORD VIDEO OF THE SIMULATION IN .MP4 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to start-recorder
  carefully [ vid:start-recorder ] [ user-message error-message ]
end

to reset-recorder
  let message (word
    "If you reset the recorder, the current recording will be lost."
    "Are you sure you want to reset the recorder?")
  if vid:recorder-status = "inactive" or user-yes-or-no? message [
    vid:reset-recorder
  ]
end

to save-recording
  if vid:recorder-status = "inactive" [
    user-message "The recorder is inactive. There is nothing to save."
    stop
  ]
  ; prompt user for movie location
  user-message (word
    "Choose a name for your movie file (the "
    ".mp4 extension will be automatically added).")
  let path user-new-file
  if not is-string? path [ stop ]  ; stop if user canceled
  ; export the movie
  carefully [
    vid:save-recording path
    user-message (word "Exported movie to " path ".")
  ] [
    user-message error-message
  ]
end
;;;
;;;;***********************************************************************************************
;;;

to pen-down-report
  ifelse pen-down?
  [ ask turtles [pen-down]]
  [ ask turtles [pen-up]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO CREATE AND DELETE OBSTACLES;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to add-obstacles
  while [mouse-down?]
    [ ask patch mouse-xcor mouse-ycor
      [ set pcolor lime + 2]
        display ]
end

to delete-obstacles
  while [mouse-down?]
    [ ask patch mouse-xcor mouse-ycor
      [ set pcolor black]
      display]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;TO CREATE FLUX PLOTS AT THE BEG AND END OF THE ENV;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to do-plots_exit
  set-current-plot "local density at exit"
plot cmax_exit
end

to output_exit
let turtle-list []
foreach my-patches
[ ?1 -> ask ?1 [ set turtle-list lput count turtles-here turtle-list ] ]
set cmax_exit sum turtle-list / 4
end

to define-square_exit
set my-patches [ ]
let i max-pxcor - 2 ;25 ; 45
let j max-pycor - 5 ;10  ; 8
while [i <= max-pxcor - 1] ;30 ; 49
[ while [j <= max-pycor - 6] ;9 ; 7
  [ set my-patches lput patch i j my-patches
    set j j + 1
    ask patch i j [set pcolor yellow + 5]
  ]
  set i i + 1
  set j 3
]
set cmax_exit 0
end

;to do-plots_beg
;  set-current-plot "local density at beg"
;plot cmax_beg
;end
;
;to output_beg
;let turtle-list_beg []
;foreach my-patches_beg
;[ ?1 -> ask ?1 [ set turtle-list_beg lput count turtles-here turtle-list_beg ] ]
;set cmax_beg sum turtle-list_beg / 11
;end
;
;to define-square_beg
;set my-patches_beg [ ]
;let i min-pxcor + 1 ;+ 3 ; + 2;; 25 ; 45
;let j min-pycor; + 1;+ 1 ;; 10  ; 8
;  while [i >= min-pxcor + 1 and i <= max-pxcor - 49] ;30 ; 49;; + 3;47
;  [ while [j >= min-pycor and j <= max-pycor - 2]; + 1];+ 3] ;9 ; 7
;  [ set my-patches_beg lput patch i j my-patches_beg
;    set j j + 1;j - 1
;    ask patch i j [set pcolor yellow + 5]
;  ]
;  set i i + 1
;  set j 2;j + 1
;]
;set cmax_beg 0
;end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;TO SAVE A PIC OF THE SIMULATION IN .PNG ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to print_PNG ; this is a button
  let fn user-new-file ; opens a dialog that allows the user to choose a location and name of a new file to be created
  ;print fn
  set fn word fn ".png"
  ;print fn
  if file-exists? fn [file-delete fn]
  file-open fn
  ;export-interface (word "view" view-number ".png") ; make sure to know at which tick the pic was taken
  export-view (word "view" view-number ".png") ; make sure to know at which tick the pic was taken
  set view-number view-number + 1
  file-close
end


to reset
  profiler:stop ;; stop profiling
  profiler:reset
  file-close
  clear-all
  reset-ticks
  clear-patches
end
@#$#@#$#@
GRAPHICS-WINDOW
549
10
2202
438
-1
-1
32.255
1
30
1
1
1
0
0
0
1
0
50
0
12
0
0
1
ticks
30.0

SLIDER
432
129
538
162
fps
fps
1
60
30.0
1
1
NIL
HORIZONTAL

SWITCH
431
360
538
393
show-FOV?
show-FOV?
1
1
-1000

SLIDER
1274
597
1408
630
Aiw
Aiw
0
100
40.0
1
1
NIL
HORIZONTAL

SLIDER
1414
597
1543
630
Biw
Biw
0
2
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
863
598
993
631
Ai
Ai
0
27
3.0
0.5
1
NIL
HORIZONTAL

SLIDER
1002
598
1132
631
Bi
Bi
0
3
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
317
242
421
275
Beta
Beta
0
2
2.0
0.5
1
NIL
HORIZONTAL

SLIDER
318
168
422
201
Tr
Tr
0
1
1.0
0.1
1
NIL
HORIZONTAL

SWITCH
317
359
423
392
pen-down?
pen-down?
1
1
-1000

BUTTON
320
9
383
42
reset
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
390
10
453
43
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
458
9
524
42
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
390
44
453
77
go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
235
159
314
204
Total turtles
count turtles
2
1
11

MONITOR
142
159
231
204
Global density
count turtles / (world-width - 1)/ (world-height - 2 )
3
1
11

PLOT
9
448
276
657
Local density at exit
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot exit_density"
"pen-1" 1.0 0 -11221820 true "" "plot mean [contagion] of turtles"

BUTTON
317
320
422
353
NIL
add-obstacles
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
430
320
540
355
NIL
delete-obstacles
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
458
44
525
77
close file
file-close
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
320
44
383
78
record
start-recorder
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
435
83
542
116
reset recording
reset-recorder
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
319
83
423
116
save recording
save-recording
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
319
121
428
166
NIL
vid:recorder-status
17
1
11

PLOT
282
448
542
657
Count turtles
NIL
NIL
0.0
1.0
0.0
200.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

SLIDER
718
598
856
631
rj_radius
rj_radius
0.5
10
3.0
0.5
1
NIL
HORIZONTAL

BUTTON
317
204
421
238
NIL
print_PNG
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1158
448
1691
593
Fswi_obs
NIL
NIL
0.0
10.0
0.0
0.05
true
true
"" ""
PENS
"mean Fsiw_obsX" 1.0 0 -13840069 true "" "carefully [plot mean [item 0 Fsi_obs] of turtles] []"
"mean Fsiw_obsY" 1.0 0 -955883 true "" "carefully [plot mean [item 1 Fsi_obs] of turtles] []"

SLIDER
430
242
538
275
threshold
threshold
0
500
200.0
10
1
NIL
HORIZONTAL

SLIDER
1158
596
1267
629
FOV_obs
FOV_obs
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
1547
634
1691
667
angle_obs
angle_obs
0
360
200.0
10
1
degree
HORIZONTAL

PLOT
10
668
542
825
Vi
NIL
NIL
0.6
10.0
0.6
0.7
true
true
"" ""
PENS
"mean Vix" 1.0 0 -10899396 true "" "carefully [plot mean [item 0 vi] of turtles] []"
"mean Viy" 1.0 0 -955883 true "" "carefully [plot mean [item 1 vi] of turtles] []"
"vdes_notsu" 1.0 0 -16777216 true "" "plot vdes_tsu"

PLOT
563
446
1128
592
Fsij
NIL
NIL
0.0
10.0
0.0
0.05
true
true
"" ""
PENS
"mean FsijX" 1.0 0 -11085214 true "" ";let asd max-one-of turtles [item 0 Fsij]\n;plot [item 0 Fsij] of asd\n;carefully [plot mean [item 0 Fsij] of turtles] []\n;let loser min-one-of turtles [item 0 Fdes]\n;plot [item 0 Fdes] of loser\n;plot min item 0 [fsij] of turtles"
"mean FsijY" 1.0 0 -955883 true "" "carefully [plot mean [item 1 Fsij] of turtles] []"

PLOT
4
8
314
155
Fdes
NIL
NIL
0.0
0.5
0.0
0.25
true
true
"" ""
PENS
"FdesX" 1.0 0 -13840069 true "" "carefully [plot mean [item 0 Fdes] of turtles] []"
"FdesY" 1.0 0 -955883 true "" "carefully [plot mean [item 1 Fdes] of turtles] []"

SLIDER
430
204
539
237
vdes_notsu
vdes_notsu
0.66
10
0.66
0.34
1
NIL
HORIZONTAL

SWITCH
317
397
422
430
Fdes_SFM?
Fdes_SFM?
0
1
-1000

SLIDER
1158
633
1268
666
FOV_walls
FOV_walls
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
1547
598
1691
631
angle_walls
angle_walls
0
360
200.0
10
1
degree
HORIZONTAL

SLIDER
1274
633
1408
666
fdd
fdd
0
1
0.156
0.001
1
NIL
HORIZONTAL

PLOT
1158
674
1692
816
Fbij
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"mean FbijX" 1.0 0 -13840069 true "" "carefully [plot mean [item 0 Fbij] of turtles] []"
"mean FbijY" 1.0 0 -955883 true "" "carefully [plot mean [item 1 Fbij] of turtles] []"

MONITOR
3
158
139
203
First turtle to reach exit
item 0 ticks-recorder
17
1
11

SLIDER
3
207
140
240
env-type
env-type
0
4
1.0
1
1
NIL
HORIZONTAL

SLIDER
565
598
710
631
FOV_radius
FOV_radius
0.5
10
3.0
0.5
1
NIL
HORIZONTAL

PLOT
1713
770
2207
914
Fbiobs
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"FbiobsX" 1.0 0 -13840069 true "" "carefully [plot mean [item 0 Fbi_obs] of turtles] []"
"FbiobsY" 1.0 0 -955883 true "" "carefully [plot mean [item 1 Fbi_obs] of turtles] []"

SWITCH
317
281
421
314
ticks-for-threshold?
ticks-for-threshold?
1
1
-1000

SLIDER
431
280
540
313
tickss
tickss
0
5000
1200.0
100
1
NIL
HORIZONTAL

PLOT
1712
448
2204
593
Fsiw
NIL
NIL
0.0
10.0
0.0
0.05
true
true
"" ""
PENS
"mean FsiwX" 1.0 0 -13840069 true "" "carefully [plot mean [item 0 Fsiw] of turtles] []"
"mean FsiwY" 1.0 0 -955883 true "" "carefully [plot mean [item 1 Fsiw] of turtles] []"

PLOT
1713
599
2205
762
Fbiw
NIL
NIL
0.0
10.0
0.0
0.05
true
true
"" ""
PENS
"mean FbiwX" 1.0 0 -13840069 true "" "carefully [plot mean [item 0 Fbiw] of turtles] []"
"mean FbiwY" 1.0 0 -955883 true "" "carefully [plot mean [item 1 Fbiw] of turtles] []"

PLOT
569
635
845
868
Density obs 1, 2, 3, 4, 5 and 6
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"Obs 1" 1.0 0 -4079321 true "" "plot density_obs1"
"Obs 2" 1.0 0 -5207188 true "" "plot density_obs2"
"Obs 3" 1.0 0 -12345184 true "" "plot density_obs3"
"Obs 4" 1.0 0 -3844592 true "" "plot density_obs4"
"Obs 5" 1.0 0 -2064490 true "" "plot density_obs5"
"Obs 6" 1.0 0 -14454117 true "" "plot density_obs6"

PLOT
857
637
1133
869
Density obs 11, 12, 13, 14 and 15
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot density_obs11"
"pen-1" 1.0 0 -5825686 true "" "plot density_obs12"
"pen-2" 1.0 0 -2674135 true "" "plot density_obs13"
"pen-3" 1.0 0 -955883 true "" "plot density_obs14"
"pen-4" 1.0 0 -6459832 true "" "plot density_obs15"

SLIDER
431
166
539
199
vdes_tsu
vdes_tsu
0
10
0.66
1
1
NIL
HORIZONTAL

PLOT
0
243
312
437
Fdes_tsu
NIL
NIL
0.0
10.0
0.0
0.25
true
false
"" ""
PENS
"Fdes_tsuX" 1.0 0 -14439633 true "" "carefully [plot mean [item 0 Fdes_tsu] of turtles] []"
"Fdes_tsuY" 1.0 0 -955883 true "" "carefully [plot mean [item 1 Fdes_tsu] of turtles] []"

SLIDER
143
207
311
240
exit-type
exit-type
0
4
0.0
1
1
NIL
HORIZONTAL

PLOT
12
830
544
979
Mean vdes
NIL
NIL
0.0
10.0
0.0
3.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [vdes] of turtles"

PLOT
1158
825
1693
977
Mean crowdedness
NIL
NIL
0.0
10.0
0.0
0.7
true
false
"" ""
PENS
"" 1.0 0 -7858858 true "" ";plot mean [num-neighbors] of turtles"
"Contagion" 1.0 0 -11221820 true "" ";plot mean [contagion] of turtles"
"Mean crowdedness" 1.0 0 -5825686 true "" "plot mean [crowd_pressure] of turtles"

SLIDER
1414
634
1544
667
angle
angle
0
360
200.0
10
1
degree
HORIZONTAL

SWITCH
430
398
538
431
Fdes_K?
Fdes_K?
1
1
-1000

@#$#@#$#@
Last update: June, 2024

## WHAT IS IT?

This is a pedestrian evacuation model in the context of a near-field tsunami evacuation considering stress levels using the social force model.

This model corresponds to Scenario 1 --> No tsunami case where agents walk at a relaxed speed.

There are environments with and without obstacles. To try different combinations of obstacles and exit width, use sliders env-type and exit-type.

## HOW IT WORKS

To start the model press setup and then go. For a new simulation press reset, setup, and go.

## THINGS TO NOTICE

Parameter values are in Table 2 in our published work: "Understanding tsunami evacuation via social force model while considering stress levels using agent-based modelling" https://www.mdpi.com/2071-1050/16/10/4307

## THINGS TO TRY

For the interaction between pedestrians, can run different simulations by varying the FOV_radius and rj_radius. For the interaction between pedestrians and obstacles/walls the same can be done by varying the FOV_obs, FOV_walls,angle_walls, and angle_obs.

To try different values for the strength and reach of the social interaction in the context of the SFM, use sliders Ai and Bi.

To try different crowd sizes; 100 or 50 (200 comes by default) move the slider threshold to the desired size, either 50 or 100. Check the number of agents with the monitor "total turtles".

To test cases with a bigger crowd size, lets say 250, you must adjust the slider tickss to 1500. Repeat the process (increase tickss) for values > 250.  Check the number of agents with the monitor "total turtles".

#######################################################################################

Use slider exit-type to test different exit configurations. exit-type = 0 comes by default.

Use slider env-type to try different obstacle settings. env-type = 0 comes by default.

env-type = 1 correspond to environment with obstacles in Flores et al. (2024)

#######################################################################################

To record, use button record, to save recording press button save recording. To reset, press button reset recording.


## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

Created in version 6.3.0 by Constanza Flores during her master's studies at Hiroshima University.

If you use this model to any extend please cite our work --> "Understanding tsunami evacuation via social force model while considering stress levels using agent-based modelling"
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

agent
true
15
Circle -1 true true 0 0 300
Line -1 true 150 150 135 0
Line -1 true 150 150 165 0
Polygon -16777216 true false 135 0 165 0 150 150 135 0

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Sensitivity analysis for sigmoid_func_beta_par_no_obs_tsu_200a" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes_tsu] of turtles</metric>
    <metric>mean [item 1 Fdes_tsu] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <metric>number</metric>
    <metric>id</metric>
    <metric>fbijxx</metric>
    <metric>fbijyy</metric>
    <enumeratedValueSet variable="tickss">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vdes_tsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beta">
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fps">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid_cao_mod?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity analysis for sigmoid_func_beta_par_obs_env1_tsu_200a" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes_tsu] of turtles</metric>
    <metric>mean [item 1 Fdes_tsu] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <metric>number</metric>
    <metric>id</metric>
    <metric>fbijxx</metric>
    <metric>fbijyy</metric>
    <enumeratedValueSet variable="tickss">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vdes_tsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beta">
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fps">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid_cao_mod?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Density-speed relationship_no_tsu_env0,exit0" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes_tsu] of turtles</metric>
    <metric>mean [item 1 Fdes_tsu] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <enumeratedValueSet variable="vdes_notsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickss">
      <value value="1100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vdes_tsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fdes_SFM?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="50 agents_no_tsu_env1_exit0_obs" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes] of turtles</metric>
    <metric>mean [item 1 Fdes] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <enumeratedValueSet variable="vdes_notsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickss">
      <value value="1100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="100 agents_no_tsu_env0_exit0" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes] of turtles</metric>
    <metric>mean [item 1 Fdes] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <enumeratedValueSet variable="vdes_notsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickss">
      <value value="1100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="100 agents_no_tsu_env1_exit0_obs" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes] of turtles</metric>
    <metric>mean [item 1 Fdes] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <enumeratedValueSet variable="vdes_notsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickss">
      <value value="1100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="200 agents_no_tsu_env0_exit0" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes] of turtles</metric>
    <metric>mean [item 1 Fdes] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <metric>density_obs1</metric>
    <metric>density_obs2</metric>
    <metric>density_obs3</metric>
    <metric>density_obs4</metric>
    <metric>density_obs5</metric>
    <metric>density_obs6</metric>
    <metric>density_obs11</metric>
    <metric>density_obs12</metric>
    <metric>density_obs13</metric>
    <metric>density_obs14</metric>
    <metric>density_obs15</metric>
    <enumeratedValueSet variable="vdes_notsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickss">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="200 agents_no_tsu_env1_exit0" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes] of turtles</metric>
    <metric>mean [item 1 Fdes] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <metric>density_obs1</metric>
    <metric>density_obs2</metric>
    <metric>density_obs3</metric>
    <metric>density_obs4</metric>
    <metric>density_obs5</metric>
    <metric>density_obs6</metric>
    <metric>density_obs11</metric>
    <metric>density_obs12</metric>
    <metric>density_obs13</metric>
    <metric>density_obs14</metric>
    <metric>density_obs15</metric>
    <enumeratedValueSet variable="vdes_notsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickss">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="50 agents_no_tsu_env0_exit0" repetitions="10" runMetricsEveryStep="true">
    <setup>reset
setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>mean [item 0 Fsij] of turtles</metric>
    <metric>mean [item 1 Fsij] of turtles</metric>
    <metric>mean [item 0 Fsiw] of turtles</metric>
    <metric>mean [item 1 Fsiw] of turtles</metric>
    <metric>mean [item 0 Fsi_obs] of turtles</metric>
    <metric>mean [item 1 Fsi_obs] of turtles</metric>
    <metric>mean [item 0 Fbij] of turtles</metric>
    <metric>mean [item 1 Fbij] of turtles</metric>
    <metric>mean [item 0 Fbiw] of turtles</metric>
    <metric>mean [item 1 Fbiw] of turtles</metric>
    <metric>mean [item 0 Fbi_obs] of turtles</metric>
    <metric>mean [item 1 Fbi_obs] of turtles</metric>
    <metric>mean [item 0 Fdes] of turtles</metric>
    <metric>mean [item 1 Fdes] of turtles</metric>
    <metric>mean [item 0 vi] of turtles</metric>
    <metric>mean [item 1 vi] of turtles</metric>
    <metric>exit_density</metric>
    <enumeratedValueSet variable="vdes_notsu">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tickss">
      <value value="1100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rj_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Tr">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aiw">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="env-type">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_obs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_walls">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Biw">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ai">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FOV_radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bi">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
