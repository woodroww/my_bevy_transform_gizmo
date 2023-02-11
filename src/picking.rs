use bevy::prelude::*;
use bevy_mod_raycast::RaycastSystem;

use crate::GizmoSystemsEnabledCriteria;

/// Used to mark the meshes and the ray casting source (camera), only these will be checked for intersections.
pub struct GizmoRaycastSet;
/// The type used on the camera to indicate it is the raycasting source.
pub type GizmoPickSource = bevy_mod_raycast::RaycastSource<GizmoRaycastSet>;
/// The type used to mark the meshes to check for intersections.
pub type PickableGizmo = bevy_mod_raycast::RaycastMesh<GizmoRaycastSet>;

/// Plugin with all the systems and resources used to raycast against gizmo handles
/// This is separate from any use of the `bevy_mod_picking` plugin, which is built on top of the bevy_mod_raycast crate.
pub struct GizmoPickingPlugin;
impl Plugin for GizmoPickingPlugin {
    fn build(&self, app: &mut App) {
        app.add_system_set_to_stage(
            CoreStage::PreUpdate,
            SystemSet::new()
                .with_run_criteria(GizmoSystemsEnabledCriteria)
                .with_system(
                    bevy_mod_raycast::build_rays::<GizmoRaycastSet>
                        .label(RaycastSystem::BuildRays::<GizmoRaycastSet>),
                )
                .with_system(
                    bevy_mod_raycast::update_raycast::<GizmoRaycastSet>
                        .label(RaycastSystem::UpdateRaycast::<GizmoRaycastSet>)
                        .after(RaycastSystem::BuildRays::<GizmoRaycastSet>),
                )
                .with_system(
                    update_gizmo_raycast_with_cursor
                        .before(RaycastSystem::BuildRays::<GizmoRaycastSet>),
                ),
        );
    }
}


/// Update the gizmo's raycasting source with the current mouse position.
fn update_gizmo_raycast_with_cursor(
    mut cursor: EventReader<CursorMoved>,
    mut query: Query<&mut GizmoPickSource>,
) {
    for mut pick_source in &mut query.iter_mut() {
        // Grab the most recent cursor event if it exists:
        if let Some(cursor_latest) = cursor.iter().last() {
            pick_source.cast_method =
                bevy_mod_raycast::RaycastMethod::Screenspace(cursor_latest.position);
        }
    }
}
