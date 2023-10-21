# See LICENSE for license details.
set_property CLOCK_DEDICATED_ROUTE BACKBONE [get_nets ip_mmcm/inst/clk_in1_mmcm]

# Place the current design
place_design -directive Explore

# Optimize the current placed netlist
phys_opt_design -directive Explore

# Optimize dynamic power using intelligent clock gating
power_opt_design

# Checkpoint the current design
write_checkpoint -force [file join $wrkdir post_place]
